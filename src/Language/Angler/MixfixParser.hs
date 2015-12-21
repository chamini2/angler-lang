{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Language.Angler.MixfixParser
        ( parseMixfix ) where

import           Language.Angler.Program
import           Language.Angler.Error       hiding (ParseError)
import           Language.Angler.Monad
import           Language.Angler.ScopedTable hiding (elem, empty, fromFoldable, toList)
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.SrcLoc

import           Control.Applicative         (Alternative(..))

import           Control.Lens                hiding (op, below, parts)

import           Control.Monad               (when)
import           Control.Monad.Except        (Except, runExcept)
import           Control.Monad.State         (State, runState)

import           Data.Function               (on)
import           Data.Default                (Default(..))

import           Data.Foldable               (foldl', foldr', toList)
import           Data.List                   (sortBy)
import           Data.Sequence               (Seq, fromList)

import           Data.Maybe                  (fromJust, fromMaybe, isJust, isNothing)

import qualified Data.Map.Strict             as Map

import           Text.Megaparsec             (ParsecT, runParserT', choice, eof, token, try)
import           Text.Megaparsec.Error       (ParseError, Message(..), errorMessages, errorPos)
import           Text.Megaparsec.Pos         (SourcePos(..), newPos)
import           Text.Megaparsec.Prim        (MonadParsec)
import qualified Text.Megaparsec.Prim        as TM
import           Text.Megaparsec.ShowToken   (ShowToken(..))

import           Debug.Trace
import           PrettyShow

--------------------------------------------------------------------------------
-- Operator handling

type OpScope = Scope Operator
type OpTable = ScopedTable Operator

type Fixity' = Fixity ()

type OperatorPart = Maybe String
type OperatorRepr = [OperatorPart]
type PrecLevel    = Map.Map Fixity' [OperatorRepr]
type PrecTable    = [PrecLevel]

data Operator
  = Operator
        { _op_idn       :: String
        , _op_fix       :: Fixity'
        }
  deriving Show

makeLenses ''Operator

opStr :: OperatorRepr -> String
opStr = concatMap (fromMaybe "_")

strOp :: String -> OperatorRepr
strOp = foldr' go []
    where
        go :: Char -> OperatorRepr -> OperatorRepr
        go c ps = case (c,ps) of
                ('_', _           ) -> Nothing    : ps
                ( _ , Just p : ps') -> Just (c:p) : ps'
                ( _ , _           ) -> Just [c]   : ps

-- Lens for accessing OperatorRepr as if it was a field of Operator
op_prts :: Lens' Operator OperatorRepr
op_prts op_fn op = wrap <$> views op_idn (op_fn . strOp) op
    where
        wrap :: OperatorRepr -> Operator
        wrap prts = set op_idn (opStr prts) op

--------------------------------------------------------------------------------
-- LoadPrecTable monad

type LoadPrecTable = State LoadPrecTableState

data LoadPrecTableState
  = LoadPrecTableState
        { _lp_table      :: OpTable
        , _lp_errors     :: [Located Error]
        }

makeLenses ''LoadPrecTableState

instance STErrors LoadPrecTableState where
        st_errors = lp_errors

instance STScopedTable LoadPrecTableState Operator where
        st_table = lp_table

instance Default LoadPrecTableState where
        def = LoadPrecTableState (ST.fromFoldable [arrowOperator]) []
            where
                arrowOperator :: (String, Operator)
                arrowOperator = (str, op)
                    where
                        str = "_->_"
                        op = Operator str (Infix RightAssoc 0 ())

parseMixfix :: ModuleSpan -> Either [Located Error] ModuleSpan
parseMixfix = handleEither . runMixfix . mixfixModule
    where
        handleEither :: (ModuleSpan, LoadPrecTableState) -> Either [Located Error] ModuleSpan
        handleEither (mdl, st) = let errs = view st_errors st
                                 in if length errs > 0 then Left errs else Right mdl

runMixfix :: LoadPrecTable a -> (a, LoadPrecTableState)
runMixfix = flip runState def

----------------------------------------

mixfixModule :: ModuleSpan -> LoadPrecTable ModuleSpan
mixfixModule = mod_body mixfixBody

mixfixBody :: BodySpan -> LoadPrecTable BodySpan
mixfixBody bdy = mapM_ loadPrecOp (view bod_stmts bdy)
              >> bod_stmts (mapM mixfixBodyStmt) bdy
    where
        loadPrecOp :: BodyStmtSpan -> LoadPrecTable ()
        loadPrecOp stmt = case stmt of
                OperatorDef idn fix spn -> do
                        let idn' = view idn_str idn
                            fix' = set fix_annot () fix
                        merr <- insertSc idn' (Operator idn' fix')
                        when (isJust merr) $ do
                                let Just err = merr
                                addError err spn
                _ -> return ()

        mixfixBodyStmt :: BodyStmtSpan -> LoadPrecTable BodyStmtSpan
        mixfixBodyStmt stmt = case stmt of
                OpenType {}     -> stmt & open_type         mixfixExprWhere
                                      >>= (open_cnts._Just) (mapM mixfixTypeBind)

                ReopenType {}   -> stmt & rpen_cnts (mapM mixfixTypeBind)

                ClosedType {}   -> stmt & clsd_type mixfixExprWhere
                                      >>= clsd_cnts (mapM mixfixTypeBind)

                FunctionDecl {} -> stmt & fdec_type mixfixExprWhere

                FunctionDef {}  -> stmt & fdef_args mixfixArgument
                                      >>= fdef_expr mixfixExprWhere

                OperatorDef {} -> return stmt

mixfixExprWhere :: ExprWhereSpan -> LoadPrecTable ExprWhereSpan
mixfixExprWhere whre = bracketSc $ whre & (whre_body._Just) mixfixBody
                                      >>= whre_insd         mixfixExpression

mixfixExpression :: ExpressionSpan -> LoadPrecTable ExpressionSpan
mixfixExpression expr = case expr of
        Var name spn -> opParse [name] spn [expr] >>= handleParseEither

        Lit {} -> return expr

        Apply xs spn -> do
                xs'  <- toList <$> mapM mixfixExpression' xs
                eith <- opParse (exprListVars xs) spn xs'
                handleParseEither (flattenExpression <$> eith)
            where
                mixfixExpression' :: ExpressionSpan -> LoadPrecTable ExpressionSpan
                mixfixExpression' expr' = case expr' of
                        Var {} -> return expr'
                        _      -> mixfixExpression expr'

                exprListVars :: Foldable f =>  f ExpressionSpan -> [String]
                exprListVars = foldl' go []
                    where
                        go :: [String] -> ExpressionSpan -> [String]
                        go parts expr' = case expr' of
                                Var name _ -> name : parts
                                _          -> parts

                flattenExpression :: ExpressionSpan -> ExpressionSpan
                flattenExpression expr' = case expr' of
                        Apply xs' an -> case toList xs' of
                                [x] -> flattenExpression x
                                _   -> Apply (fmap flattenExpression xs') an
                        _ -> expr'

        Lambda {}       -> expr & lam_arg  mixfixArgument
                              >>= lam_expr mixfixExpression

        Let {}          -> bracketSc $ expr & let_body mixfixBody
                                          >>= let_expr mixfixExpression

        Forall {}       -> expr & fall_typs (mapM mixfixTypeBind)
                              >>= fall_expr mixfixExpression

        Exists {}       -> expr & exst_type mixfixTypeBind
                              >>= exst_expr mixfixExpression

        Select {}       -> expr & slct_type mixfixTypeBind

        ImplicitExpr {} -> expr & impl_exprs (mapM mixfixImplicit)

    where
        handleParseEither :: Either ParseError ExpressionSpan -> LoadPrecTable ExpressionSpan
        handleParseEither = either handleParseError return
            where
                handleParseError :: ParseError -> LoadPrecTable ExpressionSpan
                handleParseError perr = do
                        let msgs = errorMessages perr
                            eSpn = posSpan (errorPos perr)
                        addCErr (messagesError msgs) eSpn
                        return expr

                posSpan :: SourcePos -> SrcSpan
                posSpan p = SrcSpanPoint (sourceName p) (sourceLine p) (sourceColumn p)

messagesError :: [Message] -> CheckError
messagesError = uncurry' . foldl' go ([], [], [])
    where
        go :: ([String], [String], [String]) -> Message -> ([String], [String], [String])
        go errs msg = over lns (|> str) errs
            where
                (lns, str) = case msg of
                        Expected   str -> (_1, str)
                        Unexpected str -> (_2, str)
                        Message    str -> (_3, str)

        uncurry' :: ([String], [String], [String]) -> CheckError
        uncurry' (exs, uns, mss) = CErrMixfix exs uns mss

mixfixTypeBind :: TypeBindSpan -> LoadPrecTable TypeBindSpan
mixfixTypeBind = typ_type mixfixExprWhere

mixfixArgument :: ArgumentSpan -> LoadPrecTable ArgumentSpan
mixfixArgument = (exprArg <$>) . mixfixExpression . argExpr
    where
        argExpr :: ArgumentSpan -> ExpressionSpan
        argExpr arg = case arg of
                DontCare an        -> Var "_" an
                VarBinding idn an  -> Var idn an
                ApplyBinding xs an -> Apply (fmap argExpr xs) an
        exprArg :: ExpressionSpan -> ArgumentSpan
        exprArg expr = case expr of
                -- this is a bit hard-coded, since the Var "_" cannot come from
                -- the lexer/parser, so we *know* it's the DontCare
                Var "_" an  -> DontCare an
                Var idn an  -> VarBinding idn an
                Apply xs an -> ApplyBinding (fmap exprArg xs) an
                _           -> error "MixfixParser.mixfixArgument: impossible case"

mixfixImplicit :: ImplicitBindingSpan -> LoadPrecTable ImplicitBindingSpan
mixfixImplicit = impl_expr mixfixExpression

--------------------------------------------------------------------------------
-- Expressions parser

type ExprSpan = ExpressionSpan
type OpParser = ParsecT [ExprSpan] LoadPrecTable

opParse :: [String] -> SrcSpan -> [ExprSpan] -> LoadPrecTable (Either ParseError ExprSpan)
opParse = runOpParser . generateOpParser

runOpParser :: OpParser a -> SrcSpan -> [ExprSpan] -> LoadPrecTable (Either ParseError a)
runOpParser act spn input = snd <$> runParserT' act initialState
    where
        initialState :: TM.State [ExprSpan]
        initialState = TM.State input (spanSPos spn) 8

----------------------------------------
-- DSL connections

spanSPos :: SrcSpan -> SourcePos
spanSPos spn = newPos (view spn_file spn) (view spn_sline spn) (view spn_scol spn)
spanEPos :: SrcSpan -> SourcePos
spanEPos spn = newPos (view spn_file spn) (view spn_eline spn) (view spn_ecol spn)
currentPos :: ExprSpan -> (SourcePos, SourcePos)
currentPos = views exp_annot (\spn -> (spanSPos spn, spanEPos spn))

levelFixity :: Fixity' -> PrecLevel -> [OperatorRepr]
levelFixity fix = fromMaybe [] . Map.lookup fix

mapTryChoice :: (Foldable f, Functor f, MonadParsec s m t) => f a -> (a -> m b)-> m b
mapTryChoice ops act = choice (fmap (try . act) ops)

type ConSnoc f a = (Alternative f, Cons (f a) (f a) a a, Snoc (f a) (f a) a a)
exprListSpan :: ConSnoc f ExprSpan => f ExprSpan -> SrcSpan
exprListSpan xs = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)

satisfy' :: (ExprSpan -> Either [Message] ExprSpan) -> OpParser ExprSpan
satisfy' = token currentPos'
    where
        currentPos' :: Int -> SourcePos -> ExprSpan -> SourcePos
        currentPos' _tab _p = views exp_annot spanEPos

satisfy :: [Message] -> (ExprSpan -> Bool) -> OpParser ExprSpan
satisfy errs guard = satisfy' testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if guard x
                then Right x
                else Left (Unexpected (showToken x) : errs)

var :: String -> OpParser ExprSpan
var str = satisfy [Expected str] sameVar
    where
        sameVar :: ExprSpan -> Bool
        sameVar x = case x of
                Var name _ -> name == str
                _          -> False

----------------------------------------
-- Parser generator

generateOpParser :: [String] -> OpParser ExprSpan
generateOpParser inputVars = topP <* eof
    where
        closedPartP :: ConSnoc f ExprSpan => OperatorRepr -> OpParser (SrcSpan, f ExprSpan)
        closedPartP = foldr' go (pure (SrcSpanNoInfo, empty)) . closedPart
            where
                go :: Alternative f => OperatorPart
                                    -> OpParser (SrcSpan, f ExprSpan)
                                    -> OpParser (SrcSpan, f ExprSpan)
                go mpart act = do
                        (xsp, mx) <- partP mpart
                        (asp, xs) <- act
                        return (srcSpanSpan xsp asp, mx <|> xs)
                partP :: Alternative f => OperatorPart -> OpParser (SrcSpan, f ExprSpan)
                partP mprt = case mprt of
                        Just prt -> do
                                x <- var prt
                                return (x^.exp_annot, empty)
                        Nothing  -> do
                                x <- topP
                                return (x^.exp_annot, pure x)
                closedPart :: OperatorRepr -> OperatorRepr
                closedPart = reverse . clean . reverse . clean
                    where
                        clean :: [Maybe String] -> [Maybe String]
                        clean = dropWhile isNothing

        precTable :: OpParser PrecTable
        precTable = uses st_table (buildTable . sort . filterByParts . fmap snd . ST.toList)
            where
                filterByParts :: [Operator] -> [Operator]
                filterByParts = filter hasPart
                    where
                        hasPart :: Operator -> Bool
                        hasPart = any (`elem` inputVars) . opParts
                        opParts :: Operator -> [String]
                        opParts = toListOf (op_prts.each._Just)

                sort :: [Operator] -> [Operator]
                sort = sortBy (compare `on` previewOpPrec)

                previewOpPrec :: Operator -> Maybe Int
                previewOpPrec = preview (op_fix.fix_prec)

                buildTable :: [Operator] -> PrecTable
                buildTable = fst . foldr' go ([Map.empty], Nothing)
                    where
                        go :: Operator -> (PrecTable, Maybe Int) -> (PrecTable, Maybe Int)
                        go op (tab, mprec) = (go' tab, opPrec)
                            where
                                opPrec :: Maybe Int
                                opPrec = previewOpPrec op

                                go' :: PrecTable -> PrecTable
                                go' = if opPrec < mprec
                                        then cons newLvl
                                        else over _head consOp
                                    where
                                        newLvl :: PrecLevel
                                        newLvl = uncurry Map.singleton opInfo

                                        consOp :: PrecLevel -> PrecLevel
                                        consOp = uncurry (Map.insertWith (++)) opInfo

                                        opInfo :: (Fixity (), [OperatorRepr])
                                        opInfo = (view op_fix op, [view op_prts op])

        topP :: OpParser ExprSpan
        topP = precTable >>= choice . foldr' go [bottomP]
            where
                -- keep every parser level, passing them as the *hole*
                -- parser to use in the next level
                go :: PrecLevel -> [OpParser ExprSpan] -> [OpParser ExprSpan]
                go lvl acts = try (levelP lvl acts) : acts

        bottomP :: OpParser ExprSpan
        bottomP = do
                xs <- fromList <$> (some basicToken)
                return (Apply xs (exprListSpan xs))
            where
                -- this tries to get a basic token, if we couldn't get even one,
                -- it gets any token, and then continues with the basic tokens,
                -- so `if a then if b else c` is parsed `if_then_else_ a (if b) c`
                -- instead of giving a parse error
                _cleverTokens :: OpParser [ExprSpan]
                _cleverTokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken
                    where
                        anyToken :: OpParser ExprSpan
                        anyToken = satisfy [] (const True)

                basicToken :: OpParser ExprSpan
                basicToken = try obviousToken <|> closedfixP
                    where
                        obviousToken :: OpParser ExprSpan
                        obviousToken = precTable >>= satisfy [Expected "identifier"] . notOpPart
                            where
                                notOpPart :: PrecTable -> ExprSpan -> Bool
                                notOpPart prec x = case x of
                                        Var name _ -> name `notElem` opParts
                                        _          -> True
                                    where
                                        opParts :: [String]
                                        opParts = toListOf (each.each.each.each._Just) prec

                        closedfixP :: OpParser ExprSpan
                        closedfixP = do
                                closedOps <- concatMap (levelFixity (Closedfix ())) <$> precTable
                                mapTryChoice closedOps $ \op -> do
                                        (spn, xs) <- closedPartP op
                                        let nam = opStr op
                                        return (Apply (Var nam spn <| xs) spn)

        levelP :: PrecLevel -> [OpParser ExprSpan] -> OpParser ExprSpan
        levelP lvl hole = try nonAssocP <|> try rightP <|> leftP
            where
                holeP :: OpParser ExprSpan
                holeP = choice hole

                infixOps :: Associativity -> [OperatorRepr]
                infixOps ass = levelFixity (Infix ass dummy ()) lvl
                    where
                        dummy = error "infixOps: shouldn't be evaluated"

                nonAssocP :: OpParser ExprSpan
                nonAssocP = mapTryChoice (infixOps NonAssoc) $ \op -> do
                        leftHS    <- holeP
                        (_, clxs) <- closedPartP op
                        rightHS   <- holeP
                        let spn = (srcSpanSpan `on` view exp_annot) leftHS rightHS
                            nam = opStr op
                            xs  = leftHS <| (clxs |> rightHS)
                        return (Apply (Var nam spn <| xs) spn)

                rightP :: OpParser ExprSpan
                rightP = do
                        xs <- some (try prefixP <|> rightAssocP)
                        x  <- holeP
                        return (foldr' go x xs)
                    where
                        go :: (ExprSpan, Seq ExprSpan) -> ExprSpan -> ExprSpan
                        go (op, xs) x = let xs' = xs |> x
                                        in Apply (op <| xs') (exprListSpan xs')

                        prefixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        prefixP = mapTryChoice prefixOps $ \op -> do
                                (spn, xs) <- closedPartP op
                                let nam = opStr op
                                return (Var nam spn, xs)
                            where
                                prefixOps :: [OperatorRepr]
                                prefixOps = levelFixity (Prefix dummy ()) lvl
                                    where
                                        dummy = error "prefixOps: shouldn't be evaluated"

                        rightAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        rightAssocP = mapTryChoice (infixOps RightAssoc) $ \op -> do
                                leftHS      <- holeP
                                (clsp,clxs) <- closedPartP op
                                let xs  = leftHS <| clxs
                                    spn = srcSpanSpan (leftHS^.exp_annot) clsp
                                    nam = opStr op
                                return (Var nam spn, xs)

                leftP :: OpParser ExprSpan
                leftP = do
                        x  <- holeP
                        xs <- some (try postfixP <|> leftAssocP)
                        return (foldl' go x xs)
                    where
                        go :: ExprSpan -> (ExprSpan, Seq ExprSpan) -> ExprSpan
                        go x (op, xs) = let xs' = x <| xs
                                        in Apply (op <| xs') (exprListSpan xs')

                        postfixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        postfixP = mapTryChoice postfixOps $ \op -> do
                                (spn, xs) <- closedPartP op
                                let nam = opStr op
                                return (Var nam spn, xs)
                            where
                                postfixOps :: [OperatorRepr]
                                postfixOps = levelFixity (Postfix dummy ()) lvl
                                    where
                                        dummy = error "postfixOps: shouldn't be evaluated"

                        leftAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        leftAssocP = mapTryChoice (infixOps LeftAssoc) $ \op -> do
                                (clsp, clxs) <- closedPartP op
                                rightHS      <- holeP
                                let xs  = clxs |> rightHS
                                    spn = srcSpanSpan clsp (rightHS^.exp_annot)
                                    nam = opStr op
                                return (Var nam spn, xs)
