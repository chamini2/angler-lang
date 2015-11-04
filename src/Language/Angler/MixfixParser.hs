{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Language.Angler.MixfixParser
        ( parseMixfix ) where

import           Language.Angler.AST
import           Language.Angler.Error       hiding (ParseError)
import           Language.Angler.Monad
import           Language.Angler.ScopedTable hiding (elem, empty, toList)
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Applicative         (Alternative(..))

import           Control.Lens                hiding (op, below, parts)

import           Control.Monad               (when)
import           Control.Monad.Except        (Except, runExcept)
import           Control.Monad.State         (StateT, runStateT, gets)
import           Control.Monad.Trans         (lift)

import           Data.Function               (on)
import           Data.Default                (Default(..))

import           Data.Foldable               (foldl', foldr', toList)
import           Data.List                   (sortBy)
import           Data.Sequence               (Seq, fromList)

import           Data.Maybe                  (fromJust, fromMaybe, isJust, isNothing)

import qualified Data.Map.Strict             as Map

import           Text.Megaparsec             (ParsecT, runParserT, (<|>),
                                              choice, eof, many, token, try, some)
import           Text.Megaparsec.Error       (ParseError, Message(..),
                                              errorMessages, errorPos)
import           Text.Megaparsec.Pos         (SourcePos(..), newPos)
import           Text.Megaparsec.Prim        (MonadParsec, getInput, getPosition,
                                              setInput, setPosition)
import           Text.Megaparsec.ShowToken   (ShowToken(..))

import           Debug.Trace

--------------------------------------------------------------------------------
-- Operator handling

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

type LoadPrecTableState = ScopedTable Operator
type LoadPrecTable = StateT LoadPrecTableState (Except [Located Error])

instance STScopedTable LoadPrecTableState Operator where
        st_table = id

instance Default LoadPrecTableState where
        def = ST.empty

runMixfixParser :: LoadPrecTable a -> Either [Located Error] (a, LoadPrecTableState)
runMixfixParser = runExcept . flip runStateT def

parseMixfix :: ModuleSpan -> Either [Located Error] ModuleSpan
parseMixfix = over _Right fst . runMixfixParser . mixfixModule

----------------------------------------

mixfixModule :: ModuleSpan -> LoadPrecTable ModuleSpan
mixfixModule = mod_body %%~ mixfixBody

mixfixBody :: BodySpan -> LoadPrecTable BodySpan
mixfixBody = mapM mixfixBodyStmt
    where
        mixfixBodyStmt :: BodyStmtSpan -> LoadPrecTable BodyStmtSpan
        mixfixBodyStmt stmt = case stmt of
                OpenType {}     -> stmt & open_type %%~ mixfixExprWhere
                                      >>= open_cnts._Just %%~ mapM mixfixTypeBind
                ReopenType {}   -> stmt & rpen_cnts %%~ mapM mixfixTypeBind
                ClosedType {}   -> stmt & clsd_type %%~ mixfixExprWhere
                                      >>= clsd_cnts %%~ mapM mixfixTypeBind
                FunctionDecl {} -> stmt & fdec_type %%~ mixfixExprWhere
                FunctionDef {}  -> stmt & fdef_args %%~ mixfixArgument
                                      >>= fdef_expr mixfixExprWhere

                OperatorDef idn fix spn -> do
                        let idn' = view idn_str idn
                            fix' = set fix_annot () fix
                        merr <- safeInsertSc idn' (Operator idn' fix')
                        when (isJust merr) $ do
                                let Just err = merr
                                throwError [Loc spn err]
                        return stmt

mixfixExprWhere :: ExprWhereSpan -> LoadPrecTable ExprWhereSpan
mixfixExprWhere whre = whre & whre_insd %%~ mixfixExpression
                          >>= whre_body._Just %%~ mixfixBody

mixfixExpression :: ExpressionSpan -> LoadPrecTable ExpressionSpan
mixfixExpression expr = do
        let input = [expr]
            spn   = view exp_annot expr
        eith <- runOpParser (generateOpParser (exprListVars input)) spn input
        flattenExpression <$> case eith of
                Left perr -> do
                        let msgs = errorMessages perr
                            spn  = posSpan (errorPos perr)
                        throwError (fmap (Loc spn . messageError) msgs)
                Right x -> return x
    where
        posSpan :: SourcePos -> SrcSpan
        posSpan pos = SrcSpanPoint (sourceName pos) (sourceLine pos) (sourceColumn pos)

        flattenExpression :: ExpressionSpan -> ExpressionSpan
        flattenExpression expr' = case expr' of
                Apply xs an -> case toList xs of
                        [x] -> flattenExpression x
                        _   -> Apply (fmap flattenExpression xs) an
                _ -> expr'

        messageError :: Message -> Error
        messageError msg = CheckError $ case msg of
                Expected   str -> CErrExpected str
                Unexpected str -> CErrUnexpected str
                Message    str -> CErr str

mixfixTypeBind :: TypeBindSpan -> LoadPrecTable TypeBindSpan
mixfixTypeBind = typ_type %%~ mixfixExprWhere

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
mixfixImplicit = impl_expr %%~ mixfixExpression

--------------------------------------------------------------------------------
-- Expressions parser

type ExprSpan = ExpressionSpan
type OpParser = ParsecT [ExprSpan] LoadPrecTable

runOpParser :: OpParser a -> SrcSpan -> [ExprSpan] -> LoadPrecTable (Either ParseError a)
runOpParser act spn = runParserT (setPosition (spanPos spn) >> act) filepath
    where
        filepath :: FilePath
        filepath = view spn_file spn

----------------------------------------
-- DSL connections

exprListVars :: Foldable f =>  f ExpressionSpan -> [String]
exprListVars = foldl' go []
    where
        go :: [String] -> ExpressionSpan -> [String]
        go parts expr = case expr of
                Var name _ -> name : parts
                _          -> parts

spanPos :: SrcSpan -> SourcePos
spanPos spn = newPos (view spn_file spn) (view spn_sline spn) (view spn_scol spn)

levelFixity :: Fixity' -> PrecLevel -> [OperatorRepr]
levelFixity fix = fromMaybe [] . Map.lookup fix

mapTryChoice :: (Foldable f, Functor f, MonadParsec s m t) => f a -> (a -> m b)-> m b
mapTryChoice ops act = choice (fmap (try . act) ops)

type ConSnoc f a = (Alternative f, Cons (f a) (f a) a a, Snoc (f a) (f a) a a)
exprListSpan :: ConSnoc f ExprSpan => f ExprSpan -> SrcSpan
exprListSpan xs = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)

satisfy' :: (ExprSpan -> Either [Message] ExprSpan) -> OpParser ExprSpan
satisfy' guard = token nextPos guard >>= handleExprSpan
    where
        nextPos :: Int -> SourcePos -> ExprSpan -> SourcePos
        nextPos _tab _p = spanPos . view exp_annot

        -- How to handle any specific expression
        handleExprSpan :: ExprSpan -> OpParser ExprSpan
        handleExprSpan expr = case expr of
                Var _ _ -> return expr
                Lit _ _ -> return expr
                Apply xs an -> do
                        inp <- getInput
                        pos <- getPosition

                        setInput (toList xs)
                        setPosition (spanPos an)        -- this probably does nothing
                        x' <- generateOpParser (exprListVars xs)

                        setInput inp
                        setPosition pos
                        return x'
                Lambda arg x an -> lift $ do
                        arg' <- mixfixArgument arg
                        x'   <- mixfixExpression x
                        return (Lambda arg' x' an)
                Let body x an -> lift $ do
                        body' <- mixfixBody body
                        x'    <- mixfixExpression x
                        return (Let body' x' an)
                Forall typs x an -> lift $ do
                        typs' <- mapM mixfixTypeBind typs
                        x'    <- mixfixExpression x
                        return (Forall typs' x' an)
                Exists typ x an -> lift $ do
                        typ' <- mixfixTypeBind typ
                        x'   <- mixfixExpression x
                        return (Exists typ' x' an)
                Select typ an -> lift $ do
                        typ' <- mixfixTypeBind typ
                        return (Select typ' an)
                ImplicitExpr imps an -> lift $ do
                        imps' <- mapM mixfixImplicit imps
                        return (ImplicitExpr imps' an)

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
        precTable = gets (buildTable . sort . filterByParts . fmap snd . ST.toList)
            where
                filterByParts :: [Operator] -> [Operator]
                filterByParts = filter hasPart
                    where
                        hasPart :: Operator -> Bool
                        hasPart = all (flip elem inputVars) . opPrts
                            where
                                opPrts = fmap fromJust . filter isJust . view op_prts

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
                -- This tries to get a basic token, if we couldn't get even one,
                -- it gets any token, and then continues with the basic tokens,
                -- so `if if a then b else c` is parsed `if_then_else_ (if a) b c`
                -- instead of giving a parse error
                cleverTokens :: OpParser [ExprSpan]
                cleverTokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken
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
                        let spn = (srcSpanSpan `on` (view exp_annot)) leftHS rightHS
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
