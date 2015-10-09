{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
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
import           Control.Monad               (forM_)
import           Control.Monad.State         (State, runState)
import           Control.Monad.Trans         (lift)

import           Data.Default
import           Data.Foldable               (toList)
import           Data.Function               (on)
import           Data.List                   (sortBy)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map (empty, insertWith, lookup, singleton)
import           Data.Maybe                  (isNothing)
import           Data.Sequence               (Seq, fromList)

import           Text.Megaparsec             (ParsecT, choice, eof, runParserT,
                                              token, try)
import           Text.Megaparsec.Error       (ParseError, Message(..),
                                              errorMessages, errorPos)
import           Text.Megaparsec.Pos         (SourcePos(..), setSourceName,
                                              setSourceLine, setSourceColumn)
import           Text.Megaparsec.Prim        (getInput, setInput)
import           Text.Megaparsec.ShowToken   (ShowToken(..))

import           Prelude                     hiding (lookup)

import           Debug.Trace

--------------------------------------------------------------------------------
-- Operator handling

type OperatorPart    = Maybe String                     -- Nothing represents hole
type OperatorParts   = [ OperatorPart ]
type PrecedenceLevel = Map (Fixity ()) [ OperatorParts ]
type PrecedenceTable = [ PrecedenceLevel ]

opStr :: OperatorParts -> String
opStr = concatMap (maybe "_" id)

strOp :: String -> OperatorParts
strOp = foldr go []
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
        , _op_prec      :: Maybe Int
        }
  deriving Show

-- Lens for accessing OperatorParts as if it was a field of Operator
op_repr :: Lens' Operator OperatorParts
op_repr op_fn (Operator idn fix prec) = wrap <$> op_fn (strOp idn)
    where
        wrap :: OperatorParts -> Operator
        wrap prts = Operator (opStr prts) fix prec

data OpPState
  = OpPState
        { _opp_table     :: ScopedTable Operator
        , _opp_errs      :: [ Located Error ]
        }

makeLenses ''Operator
makeLenses ''OpPState

instance STScopedTable OpPState Operator where
        st_table = opp_table

instance Default OpPState where
        def = OpPState
                { _opp_table = ST.empty
                , _opp_errs  = []
                }

scopedTabPrecedenceTab :: ScopedTable Operator -> PrecedenceTable
scopedTabPrecedenceTab = buildTable . sort . fmap snd . ST.toList
    where
        sort :: [Operator] -> [Operator]
        sort = sortBy (compare `on` view op_prec)

        buildTable :: [Operator] -> PrecedenceTable
        buildTable = fst . foldr go ([Map.empty], Nothing)

        go :: Operator -> (PrecedenceTable, Maybe Int) -> (PrecedenceTable, Maybe Int)
        go op (tab, mprec) = (go' tab, opPrec)
            where
                opPrec :: Maybe Int
                opPrec = view op_prec op

                go' :: PrecedenceTable -> PrecedenceTable
                go' = if opPrec < mprec
                        then cons newLvl
                        else over _head addOp

                newLvl :: PrecedenceLevel
                newLvl = uncurry Map.singleton opInfo

                addOp :: PrecedenceLevel -> PrecedenceLevel
                addOp = uncurry (Map.insertWith (++)) opInfo

                opInfo :: (Fixity (), [OperatorParts])
                opInfo = (view op_fix op, [view op_repr op])

--------------------------------------------------------------------------------
-- Monad

type Mixfix = State OpPState

runMixfix :: Mixfix a -> (a, OpPState)
runMixfix = flip runState def

parseMixfix :: ModuleSpan -> (ModuleSpan, [ Located Error ])
parseMixfix = over _2 (view opp_errs) . runMixfix . mixfixModule

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
        OperatorDef idn fix mprec spn -> do
                let idn' = view idn_str idn
                    fix' = set fix_annot () fix
                merr <- safeInsertSc idn' (Operator idn' fix' mprec)
                case merr of
                        Just err -> pushM opp_errs (Loc spn err)
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
                        forM_ msgs (pushM opp_errs . Loc spn . CheckError . CErr . show)
                        return expr
                Right x -> return x
        return (flattenExpression expr')
    where
        parse :: [ExprSpan] -> Mixfix (Either ParseError ExprSpan)
        parse = runOpP generateOpP (views exp_annot srcSpanFile expr)
        flattenExpression :: ExpressionSpan -> ExpressionSpan
        flattenExpression x = case x of
                Apply xs an -> case toList xs of
                        [x'] -> flattenExpression x'
                        xs'  -> Apply (fromList (fmap flattenExpression xs')) an
                _ -> x

mixfixTypeBind :: TypeBindSpan -> Mixfix TypeBindSpan
mixfixTypeBind = mapMOf typ_type mixfixWhere

mixfixArgument :: ArgumentSpan -> Mixfix ArgumentSpan
mixfixArgument arg = do
        expr <- mixfixExpression (argExpr arg)
        return (exprArg expr)
    where
        argExpr :: ArgumentSpan -> ExpressionSpan
        argExpr arg = case arg of
                VarBinding idn an  -> Var idn an
                DontCare an        -> Lit (error "mixfixArgument: DontCare") an
                ApplyBinding xs an -> Apply (fmap argExpr xs) an
        exprArg :: ExpressionSpan -> ArgumentSpan
        exprArg expr = case expr of
                Apply xs an -> case toList xs of
                        [x] -> exprArg x
                        _   -> ApplyBinding (fmap exprArg xs) an
                Var idn an -> VarBinding idn an
                Lit _   an -> DontCare an
                _          -> error "mixfixArgument: impossible case"

mixfixImplicit :: ImplicitBindingSpan -> Mixfix ImplicitBindingSpan
mixfixImplicit = return

--------------------------------------------------------------------------------
-- Parser

type ExprSpan = ExpressionSpan
type OpP = ParsecT [ExprSpan] Mixfix

runOpP :: OpP a -> FilePath -> [ExprSpan] -> Mixfix (Either ParseError a)
runOpP = runParserT

instance Show a => ShowToken (Expression a) where
        showToken = prettyShow

instance ShowToken a => ShowToken [a] where
        showToken = show

satisfy' :: (ExprSpan -> Either [Message] ExprSpan) -> OpP ExprSpan
satisfy' = token nextPos
    where
        nextPos :: Int -> SourcePos -> ExprSpan -> SourcePos
        nextPos _tab p x = (setName . setLine . setColumn) p
            where
                xSpan :: SrcSpan
                xSpan = view exp_annot x
                setName :: SourcePos -> SourcePos
                setName = flip setSourceName (srcSpanFile xSpan)
                setLine :: SourcePos -> SourcePos
                setLine = flip setSourceLine (srcSpanSLine xSpan)
                setColumn :: SourcePos -> SourcePos
                setColumn = flip setSourceColumn (srcSpanSCol xSpan)

satisfy :: (ExprSpan -> Bool) -> OpP ExprSpan
satisfy g = satisfy' testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if g x
                then Right x
                else (Left . return . Unexpected . showToken) x

var :: String -> OpP ExprSpan
var str = satisfy testExpr
    where
        testExpr :: ExprSpan -> Bool
        testExpr x = case x of
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
        topOpP = precTable >>= choice . foldr go [bottomOpP] . fmap levelOpP
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
                -- with 'if if a then b else c' it parses 'if_then_else_ (if a) b c'
                -- instead of giving a parse error
                -- This behaviour may or may not be recommended
                cleverTokens :: OpP [ExprSpan]
                cleverTokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken

                anyToken :: OpP ExprSpan
                anyToken = satisfy (const True)

                basicToken :: OpP ExprSpan
                basicToken = try obviousToken <|> closedOpP
                    where
                        obviousToken :: OpP ExprSpan
                        obviousToken = precTable >>= satisfy . testExpr >>= retake
                            where
                                testExpr :: PrecedenceTable -> ExprSpan -> Bool
                                testExpr prec x = case x of
                                        Var name _ -> name `notElem` (parts prec)
                                        _          -> True
                                parts :: PrecedenceTable -> [String]
                                parts = toListOf (traverse.traverse.traverse.traverse._Just)
                                retake :: ExprSpan -> OpP ExprSpan
                                retake expr = case expr of
                                        Var _ _ -> return expr
                                        Lit _ _ -> return expr
                                        Apply xs _ -> do
                                                input <- getInput
                                                setInput (toList xs)
                                                x' <- generateOpP
                                                setInput input
                                                return x'
                                        Lambda arg x an -> do
                                                arg' <- lift (mixfixArgument arg)
                                                x'   <- lift (mixfixExpression x)
                                                return (Lambda arg' x' an)
                                        Let body x an -> do
                                                body' <- lift (mixfixBody body)
                                                x'    <- lift (mixfixExpression x)
                                                return (Let body' x' an)
                                        Forall typs x an -> do
                                                typs' <- lift (mapM mixfixTypeBind typs)
                                                x'    <- lift (mixfixExpression x)
                                                return (Forall typs' x' an)
                                        Exists typ x an -> do
                                                typ' <- lift (mixfixTypeBind typ)
                                                x'   <- lift (mixfixExpression x)
                                                return (Exists typ' x' an)
                                        Select typ an -> do
                                                typ' <- lift (mixfixTypeBind typ)
                                                return (Select typ' an)
                                        ImplicitExpr xs an -> do
                                                xs' <- lift (mapM mixfixImplicit xs)
                                                return (ImplicitExpr xs' an)

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
                        nonOps = searchOps (Infix NonAssoc ()) lvl

                nonInfixOpP :: Fixity () -> OpP (Seq ExprSpan, SrcSpan)
                nonInfixOpP fix = tryOpsOpP (searchOps fix lvl) $ \op -> do
                        (clxs, sp, opName) <- closedPartOpP op
                        return (Var opName sp <| clxs, sp)

                rightOpP :: OpP ExprSpan
                rightOpP = do
                        xs <- some (try prefixOpP <|> rightAssocOpP)
                        x  <- belowOpP
                        return (foldr go x xs)
                    where
                        go :: (Seq ExprSpan, SrcSpan) -> ExprSpan -> ExprSpan
                        go (xs, sp) x = Apply (xs |> x) (srcSpanSpan sp (x^.exp_annot))

                        prefixOpP :: OpP (Seq ExprSpan, SrcSpan)
                        prefixOpP = nonInfixOpP (Prefix ())

                        rightAssocOpP :: OpP (Seq ExprSpan, SrcSpan)
                        rightAssocOpP = tryOpsOpP rightOps $ \op -> do
                                leftH                <- belowOpP
                                (clxs, clsp, opName) <- closedPartOpP op
                                let sp = srcSpanSpan (leftH^.exp_annot) clsp
                                return (Var opName sp <| pure leftH <|> clxs, sp)
                            where
                                rightOps :: [ OperatorParts ]
                                rightOps = searchOps (Infix RightAssoc ()) lvl

                leftOpP :: OpP ExprSpan
                leftOpP = do
                        x  <- belowOpP
                        xs <- many (try postfixOpP <|> leftAssocOpP)
                        return (foldl go x xs)
                    where
                        go :: ExprSpan -> (Seq ExprSpan, SrcSpan) -> ExprSpan
                        go x (xsS, sp) = Apply (fromList xs) sp
                            where
                                xs  = head xs' : x : tail xs'
                                xs' = toList xsS

                        postfixOpP :: OpP (Seq ExprSpan, SrcSpan)
                        postfixOpP = nonInfixOpP (Postfix ())

                        leftAssocOpP :: OpP (Seq ExprSpan, SrcSpan)
                        leftAssocOpP = tryOpsOpP leftOps $ \op -> do
                                (clxs, clsp, opName) <- closedPartOpP op
                                rightH               <- belowOpP
                                let sp = srcSpanSpan clsp (rightH^.exp_annot)
                                return (Var opName sp <| (clxs |> rightH), sp)
                            where
                                leftOps :: [ OperatorParts ]
                                leftOps = searchOps (Infix LeftAssoc ()) lvl

        closedPartOpP :: (Alternative f,
                          Cons (f ExprSpan) (f ExprSpan) ExprSpan ExprSpan,
                          Snoc (f ExprSpan) (f ExprSpan) ExprSpan ExprSpan)
                      => OperatorParts -> OpP (f ExprSpan, SrcSpan, String)
        closedPartOpP prts = do
                xs <- foldr go (pure empty) (closedPart prts)
                return (xs, xsSpan xs, opStr prts)
            where
                go :: Alternative f => OperatorPart -> OpP (f ExprSpan) -> OpP (f ExprSpan)
                go mprt act = (<|>) <$> prtOpP mprt <*> act
                prtOpP :: Alternative f => OperatorPart -> OpP (f ExprSpan)
                prtOpP mprt = case mprt of
                        Just prt -> pure empty <* var prt
                        _        -> pure <$> topOpP
                closedPart :: OperatorParts -> OperatorParts
                closedPart = reverse . clean . reverse . clean
                    where
                        clean = dropWhile isNothing

        xsSpan :: (Alternative f,
                   Cons (f ExprSpan) (f ExprSpan) ExprSpan ExprSpan,
                   Snoc (f ExprSpan) (f ExprSpan) ExprSpan ExprSpan)
               => f ExprSpan -> SrcSpan
        xsSpan xs = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)
