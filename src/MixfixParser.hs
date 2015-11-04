{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Angler.MixfixParser
        ( parseMixfix ) where

import           Language.Angler.AST
import           Language.Angler.Error       hiding (ParseError)
import           Language.Angler.Monad
import           Language.Angler.ScopedTable hiding (empty, toList)
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Applicative         (Alternative(..), (<|>), liftA, many)
import           Control.Lens                hiding (op, below, parts)
import           Control.Monad.Except        (Except, runExcept)
import           Control.Monad.State         (StateT, runStateT)
import           Control.Monad.Trans         (lift)

import           Data.Char                   (isSpace)
import           Data.Default                (Default(..))
import           Data.Foldable               (foldl', foldr', toList)
import           Data.Function               (on)
import           Data.List                   (sortBy)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isNothing)
import           Data.Sequence               (Seq, fromList)

import           Text.Megaparsec             (ParsecT, choice, eof, runParserT,
                                              token, try)
import           Text.Megaparsec.Error       (ParseError, Message(..),
                                              errorMessages, errorPos)
import           Text.Megaparsec.Pos         (SourcePos(..), newPos, setSourceName,
                                              setSourceLine, setSourceColumn)
import           Text.Megaparsec.Prim        (getInput, setInput,
                                              getPosition, setPosition)
import           Text.Megaparsec.ShowToken   (ShowToken(..))

import           Prelude                     hiding (lookup)

import           GHC.Stack
import           Debug.Trace

--------------------------------------------------------------------------------
-- Contraint Kind

type ConSnoc f a = (Alternative f, Cons (f a) (f a) a a, Snoc (f a) (f a) a a)

--------------------------------------------------------------------------------
-- Operator handling

type OperatorPart    = Maybe String                     -- Nothing represents hole
type OperatorParts   = [ OperatorPart ]
type PrecedenceLevel = Map (Fixity ()) [ OperatorParts ]
type PrecedenceTable = [ PrecedenceLevel ]

opStr :: OperatorParts -> String
opStr = concatMap (maybe "_" id)

strOp :: String -> OperatorParts
strOp = foldr' go []
    where
        go :: Char -> OperatorParts -> OperatorParts
        go c ps = case (c,ps) of
                ('_', _           ) -> Nothing    : ps
                ( _ , Just p : ps') -> Just (c:p) : ps'
                ( _ , _           ) -> Just [c]   : ps

--------------------------------------------------------------------------------
-- State

data Operator
  = Operator
        { _op_idn       :: String
        , _op_fix       :: Fixity ()
        }
  deriving Show

data OpPState
  = OpPState
        { _opp_table    :: ScopedTable Operator }

makeLenses ''Operator
makeLenses ''OpPState

-- Lens for accessing OperatorParts as if it was a field of Operator
op_repr :: Lens' Operator OperatorParts
op_repr op_fn op = wrap <$> views op_idn (op_fn . strOp) op
    where
        wrap :: OperatorParts -> Operator
        wrap prts = set op_idn (opStr prts) op

instance STScopedTable OpPState Operator where
        st_table = opp_table

instance Default OpPState where
        def = OpPState { _opp_table = ST.empty }

scopedTabPrecedenceTab :: ScopedTable Operator -> PrecedenceTable
scopedTabPrecedenceTab = buildTable . sort . fmap snd . ST.toList
    where
        sort :: [Operator] -> [Operator]
        sort = sortBy (compare `on` opPrec)

        opPrec :: Operator -> Maybe Int
        opPrec = preview (op_fix.fix_prec)

        buildTable :: [Operator] -> PrecedenceTable
        buildTable = fst . foldr' go ([Map.empty], Nothing)

        go :: Operator -> (PrecedenceTable, Maybe Int) -> (PrecedenceTable, Maybe Int)
        go op (tab, mprec) = (go' tab, opPrec op)
            where
                go' :: PrecedenceTable -> PrecedenceTable
                go' = if opPrec op < mprec
                        then cons newLvl
                        else over _head addOp
                    where
                        newLvl :: PrecedenceLevel
                        newLvl = uncurry Map.singleton opInfo

                        addOp :: PrecedenceLevel -> PrecedenceLevel
                        addOp = uncurry (Map.insertWith (++)) opInfo

                        opInfo :: (Fixity (), [OperatorParts])
                        opInfo = (view op_fix op, [view op_repr op])

--------------------------------------------------------------------------------
-- Monad

type Mixfix = StateT OpPState (Except [Located Error])

runMixfix :: Mixfix a -> Either [Located Error] (a, OpPState)
runMixfix = runExcept . flip runStateT def

parseMixfix :: ModuleSpan -> Either [Located Error] ModuleSpan
parseMixfix = over _Right fst . runMixfix . mixfixModule

--------------------------------------------------------------------------------

mixfixModule :: ModuleSpan -> Mixfix ModuleSpan
mixfixModule = mapMOf mod_body mixfixBody

mixfixBody :: BodySpan -> Mixfix BodySpan
mixfixBody = mapMOf traverse mixfixBodyStmt

mixfixBodyStmt :: BodyStmtSpan -> Mixfix BodyStmtSpan
mixfixBodyStmt stmt = case stmt of
        OpenType {} -> do
                stmt' <- mapMOf (open_cnts._Just.traverse) mixfixTypeBind stmt
                mapMOf open_type mixfixWhere stmt'
        ReopenType {} -> mapMOf (rpen_cnts.traverse) mixfixTypeBind stmt
        ClosedType {} -> do
                stmt'  <- mapMOf clsd_type mixfixWhere stmt
                mapMOf (clsd_cnts.traverse) mixfixTypeBind stmt'
        FunctionDecl {} -> mapMOf fdec_type mixfixWhere stmt
        FunctionDef {} -> do
                stmt' <- mapMOf fdef_args mixfixArgument stmt
                mapMOf fdef_expr mixfixWhere stmt'
        OperatorDef idn fix spn -> do
                let idn' = view idn_str idn
                    fix' = set fix_annot () fix
                merr <- safeInsertSc idn' (Operator idn' fix')
                case merr of
                        Just err -> throwError [Loc spn err]
                        _        -> return ()
                return stmt

mixfixWhere :: ExprWhereSpan -> Mixfix (ExprWhereSpan)
mixfixWhere whre = do
        whre' <- mapMOf (whre_body._Just) mixfixBody whre
        mapMOf whre_insd mixfixExpression whre'

mixfixExpression :: ExpressionSpan -> Mixfix ExpressionSpan
mixfixExpression expr = do
        eit   <- parse [expr]
        expr' <- case eit of
                Left perr -> do
                        let msgs = errorMessages perr
                            pos  = errorPos perr
                            loc  = SrcLoc (sourceName pos) (sourceLine pos) (sourceColumn pos)
                            spn  = srcLocSpan loc loc
                            errs = fmap (Loc spn . CheckError . messageCheckError) msgs
                        throwError errs
                Right x -> return x
        return (flattenExpression expr')
    where
        parse :: [ExprSpan] -> Mixfix (Either ParseError ExprSpan)
        parse = runOpP generateOpP (view exp_annot expr)
        flattenExpression :: ExpressionSpan -> ExpressionSpan
        flattenExpression x = case x of
                Apply xs an -> case toList xs of
                        [x'] -> flattenExpression x'
                        xs'  -> Apply (fromList (fmap flattenExpression xs')) an
                _ -> x
        messageCheckError :: Message -> CheckError
        messageCheckError msg = case msg of
                Expected   str -> CErrExpected str
                Unexpected str -> CErrUnexpected str
                Message    str -> CErr str

mixfixTypeBind :: TypeBindSpan -> Mixfix TypeBindSpan
mixfixTypeBind = mapMOf typ_type mixfixWhere

mixfixArgument :: ArgumentSpan -> Mixfix ArgumentSpan
mixfixArgument = (exprArg <$>) . mixfixExpression . argExpr
    where
        argExpr :: ArgumentSpan -> ExpressionSpan
        argExpr arg = case arg of
                VarBinding idn an  -> Var idn an
                DontCare an        -> Lit (error "MixfixParser.mixfixArgument: DontCare") an
                ApplyBinding xs an -> Apply (fmap argExpr xs) an
        exprArg :: ExpressionSpan -> ArgumentSpan
        exprArg expr = case expr of
                Apply xs an -> case toList xs of
                        [x] -> exprArg x
                        _   -> ApplyBinding (fmap exprArg xs) an
                Var idn an -> VarBinding idn an
                Lit _   an -> DontCare an
                _          -> error "MixfixParser.mixfixArgument: impossible case"

mixfixImplicit :: ImplicitBindingSpan -> Mixfix ImplicitBindingSpan
mixfixImplicit = mapMOf impl_expr mixfixExpression

--------------------------------------------------------------------------------
-- Parser

type ExprSpan = ExpressionSpan
type OpP = ParsecT [ExprSpan] Mixfix

runOpP :: OpP a -> SrcSpan -> [ExprSpan] -> Mixfix (Either ParseError a)
runOpP act spn = runParserT (setPosition pos >> act) filepath
    where
        filepath :: FilePath
        filepath = view spn_file spn
        pos :: SourcePos
        pos = newPos filepath (view spn_sline spn) (view spn_scol spn)

instance Show a => ShowToken (Expression a) where
        showToken = trim . prettyShow
            where
                trim :: String -> String
                trim = reverse . dropWhile isSpace . reverse

instance ShowToken a => ShowToken [a] where
        showToken = show

satisfy' :: (ExprSpan -> Either [Message] ExprSpan) -> OpP ExprSpan
satisfy' g = token nextPos g >>= handleExprSpan
    where
        nextPos :: Int -> SourcePos -> ExprSpan -> SourcePos
        nextPos _tab p x = (setName . setLine . setColumn) p
            where
                xSpan :: SrcSpan
                xSpan = view exp_annot x
                setName :: SourcePos -> SourcePos
                setName = flip setSourceName (view spn_file xSpan)
                setLine :: SourcePos -> SourcePos
                setLine = flip setSourceLine (view spn_sline xSpan)
                setColumn :: SourcePos -> SourcePos
                setColumn = flip setSourceColumn (view spn_scol xSpan)
        -- How to handle any specific expression
        handleExprSpan :: ExprSpan -> OpP ExprSpan
        handleExprSpan expr = case expr of
                Var _ _ -> return expr
                Lit _ _ -> return expr
                Apply xs an -> do
                        input <- getInput
                        pos   <- getPosition
                        let file = view spn_file  an
                            lin  = view spn_sline an
                            col  = view spn_scol  an
                        setInput (toList xs)
                        setPosition (newPos file lin col)
                        x' <- generateOpP
                        setInput input
                        setPosition pos
                        return x'
                Lambda {} -> lift $ do
                        expr' <- mapMOf lam_arg mixfixArgument expr
                        mapMOf lam_expr mixfixExpression expr'
                -- Lambda arg x an -> do
                --         arg' <- lift (mixfixArgument arg)
                --         x'   <- lift (mixfixExpression x)
                --         return (Lambda arg' x' an)
                Let {} -> lift $ do
                        expr' <- mapMOf let_body mixfixBody expr
                        mapMOf let_expr mixfixExpression expr'
                -- Let body x an -> do
                --         body' <- lift (mixfixBody body)
                --         x'    <- lift (mixfixExpression x)
                --         return (Let body' x' an)
                Forall {} -> lift $ do
                        expr' <- mapMOf (fall_typs.traverse) mixfixTypeBind expr
                        mapMOf fall_expr mixfixExpression expr'
                -- Forall typs x an -> do
                --         typs' <- lift (mapM mixfixTypeBind typs)
                --         x'    <- lift (mixfixExpression x)
                --         return (Forall typs' x' an)
                Exists {} -> lift $ do
                        expr' <- mapMOf exst_type mixfixTypeBind expr
                        mapMOf exst_expr mixfixExpression expr'
                -- Exists typ x an -> do
                --         typ' <- lift (mixfixTypeBind typ)
                --         x'   <- lift (mixfixExpression x)
                --         return (Exists typ' x' an)
                Select {} -> lift (mapMOf slct_type mixfixTypeBind expr)
                -- Select typ an -> do
                --         typ' <- lift (mixfixTypeBind typ)
                --         return (Select typ' an)
                ImplicitExpr {} ->
                        lift (mapMOf (impl_exprs.traverse) mixfixImplicit expr)

satisfy :: (ExprSpan -> Bool) -> OpP ExprSpan
satisfy g = satisfy' testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if g x
                then Right x
                else Left [Unexpected (showToken x)]

var :: String -> OpP ExprSpan
var str = satisfy' testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if testVar
                then Right x
                else Left [ Unexpected (showToken x)
                          , Expected str ]
            where
                testVar :: Bool
                testVar = case x of
                        Var name _ -> name == str
                        _          -> False

searchOps :: Fixity () -> PrecedenceLevel -> [ OperatorParts ]
searchOps fix = maybe [] id . Map.lookup fix

generateOpP :: OpP ExprSpan
generateOpP = topOpP <* eof
    where
        precTable :: OpP PrecedenceTable
        precTable = uses opp_table scopedTabPrecedenceTab

        topOpP :: OpP ExprSpan
        topOpP = precTable >>= choice . foldr' go [bottomOpP] . fmap levelOpP
            where
                -- We pass all the parsers below as *fallback*
                -- if we couldn't get a match on this level
                go :: ([OpP ExprSpan] -> OpP ExprSpan) -> [OpP ExprSpan] -> [OpP ExprSpan]
                go pp acts = try (pp acts) : acts

        tryOpsOpP :: [ OperatorParts ] -> (OperatorParts -> OpP a)-> OpP a
        tryOpsOpP ops act = choice (fmap (try . act) ops)

        bottomOpP :: OpP ExprSpan
        bottomOpP = do
                xs <- liftA fromList (some basicToken)
                return (Apply xs (xsSpan xs))
            where
                -- This tries to get a basic token, if we couldn't get even one,
                -- it gets any token, and then continues with the basic tokens,
                -- so `if if a then b else c` is parsed `if_then_else_ (if a) b c`
                -- instead of giving a parse error
                cleverTokens :: OpP [ExprSpan]
                cleverTokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken

                anyToken :: OpP ExprSpan
                anyToken = satisfy (const True)

                basicToken :: OpP ExprSpan
                basicToken = try obviousToken <|> closedOpP
                    where
                        obviousToken :: OpP ExprSpan
                        obviousToken = precTable >>= satisfy' . testExpr
                            where
                                testExpr :: PrecedenceTable -> ExprSpan -> Either [Message] ExprSpan
                                testExpr prec x = case x of
                                        Var name _ -> if name `notElem` (parts prec)
                                                then Right x
                                                else Left [ Unexpected ("operator part " ++ name)
                                                          , Expected "identifier" ]
                                        _ -> Right x
                                parts :: PrecedenceTable -> [String]
                                parts = toListOf (traverse.traverse.traverse.traverse._Just)
                closedOpP :: OpP ExprSpan
                closedOpP = do
                        closedOps <- concatMap (searchOps (Closedfix ())) <$> precTable
                        tryOpsOpP closedOps $ \op -> do
                                (clxs, sp, opName) <- closedPartOpP op
                                return (Apply (Var opName sp <| clxs) sp)

        levelOpP :: PrecedenceLevel -> [OpP ExprSpan] -> OpP ExprSpan
        levelOpP lvl below = try middleOpP <|> try rightOpP <|> leftOpP
            where
                belowOpP :: OpP ExprSpan
                belowOpP = choice below

                middleOpP :: OpP ExprSpan
                middleOpP = tryOpsOpP nonOps $ \op -> do
                        leftH             <- belowOpP
                        (clxs, _, opName) <- closedPartOpP op
                        rightH            <- belowOpP
                        let sp = srcSpanSpan (leftH^.exp_annot) (rightH^.exp_annot)
                            xs = Var opName sp <| pure leftH <|> clxs |> rightH
                        return (Apply xs sp)
                    where
                        nonOps :: [ OperatorParts ]
                        nonOps = searchOps (Infix NonAssoc 0 ()) lvl

                nonInfixOpP :: Fixity () -> OpP (Seq ExprSpan, SrcSpan)
                nonInfixOpP fix = tryOpsOpP (searchOps fix lvl) $ \op -> do
                        (clxs, sp, opName) <- closedPartOpP op
                        return (Var opName sp <| clxs, sp)

                rightOpP :: OpP ExprSpan
                rightOpP = do
                        xs <- some (try prefixOpP <|> rightAssocOpP)
                        x  <- belowOpP
                        return (foldr' go x xs)
                    where
                        go :: (Seq ExprSpan, SrcSpan) -> ExprSpan -> ExprSpan
                        go (xs, sp) x = Apply (xs |> x) (srcSpanSpan sp (x^.exp_annot))

                        prefixOpP :: OpP (Seq ExprSpan, SrcSpan)
                        prefixOpP = nonInfixOpP (Prefix 0 ())

                        rightAssocOpP :: OpP (Seq ExprSpan, SrcSpan)
                        rightAssocOpP = tryOpsOpP rightOps $ \op -> do
                                leftH                <- belowOpP
                                (clxs, clsp, opName) <- closedPartOpP op
                                let sp = srcSpanSpan (leftH^.exp_annot) clsp
                                return (Var opName sp <| pure leftH <|> clxs, sp)
                            where
                                rightOps :: [ OperatorParts ]
                                rightOps = searchOps (Infix RightAssoc 0 ()) lvl

                leftOpP :: OpP ExprSpan
                leftOpP = do
                        x  <- belowOpP
                        xs <- some (try postfixOpP <|> leftAssocOpP)
                        return (foldl' go x xs)
                    where
                        go :: ExprSpan -> (Seq ExprSpan, SrcSpan) -> ExprSpan
                        go x (xsS, sp) = Apply (fromList xs) sp
                            where
                                xs  = head xs' : x : tail xs'
                                xs' = toList xsS

                        postfixOpP :: OpP (Seq ExprSpan, SrcSpan)
                        postfixOpP = nonInfixOpP (Postfix 0 ())

                        leftAssocOpP :: OpP (Seq ExprSpan, SrcSpan)
                        leftAssocOpP = tryOpsOpP leftOps $ \op -> do
                                (clxs, clsp, opName) <- closedPartOpP op
                                rightH               <- belowOpP
                                let sp = srcSpanSpan clsp (rightH^.exp_annot)
                                return (Var opName sp <| (clxs |> rightH), sp)
                            where
                                leftOps :: [ OperatorParts ]
                                leftOps = searchOps (Infix LeftAssoc 0 ()) lvl

        closedPartOpP :: ConSnoc f ExprSpan => OperatorParts -> OpP (f ExprSpan, SrcSpan, String)
        closedPartOpP prts = do
                (spn, xs) <- foldr' go (pure (SrcSpanNoInfo, empty)) (closedPart prts)
                return (xs, spn, opStr prts)
            where
                go :: Alternative f => OperatorPart -> OpP (SrcSpan, f ExprSpan) -> OpP (SrcSpan, f ExprSpan)
                go mpart act = do
                        (xsp, mx) <- partP mpart
                        (asp, xs) <- act
                        return (srcSpanSpan xsp asp, mx <|> xs)
                partP :: Alternative f => OperatorPart -> OpP (SrcSpan, f ExprSpan)
                partP mprt = case mprt of
                        Just prt -> do
                                x <- var prt
                                return (x^.exp_annot, empty)
                        Nothing  -> do
                                x <- topOpP
                                return (x^.exp_annot, pure x)
                closedPart :: OperatorParts -> OperatorParts
                closedPart = reverse . clean . reverse . clean
                    where
                        clean = dropWhile isNothing

        xsSpan :: ConSnoc f ExprSpan => f ExprSpan -> SrcSpan
        xsSpan xs = srcSpanSpan ((test xs)^?!_head.exp_annot) ((test xs)^?!_last.exp_annot)
            where
                test xs = if isNothing (xs^?_head) then errorWithStackTrace "omg" else xs