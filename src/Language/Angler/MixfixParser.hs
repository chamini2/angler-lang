{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Angler.MixfixParser where

import           Language.Angler.AST
import           Language.Angler.Error       hiding (ParseError)
import           Language.Angler.Monad
import           Language.Angler.ScopedTable hiding (empty, toList)
import qualified Language.Angler.ScopedTable as ST
import           Language.Angler.SrcLoc

import           PrettyShow

import           Control.Applicative         (Alternative(..))

import           Control.Lens                hiding (op, below, parts)

import           Control.Monad.Except        (Except, runExcept)
import           Control.Monad.State         (StateT, runStateT, gets)
import           Control.Monad.Trans         (lift)

import           Data.Function               (on)

import           Data.Foldable               (toList)
import           Data.List                   (sortBy)
import           Data.Sequence               (Seq, fromList)

import           Data.Maybe                  (isNothing)

import qualified Data.Map.Strict             as Map

import           Text.Megaparsec             (ParsecT, runParserT, (<|>),
                                              choice, eof, many, token, try, some)
import           Text.Megaparsec.Error       (ParseError, Message(..))
import           Text.Megaparsec.Pos         (SourcePos, newPos)
import           Text.Megaparsec.Prim        (MonadParsec, getInput, getPosition,
                                              setInput, setPosition)
import           Text.Megaparsec.ShowToken   (ShowToken(..))

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
opStr = concatMap (maybe "_" id)

strOp :: String -> OperatorRepr
strOp = foldr go []
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

spanPos :: SrcSpan -> SourcePos
spanPos spn = newPos (view spn_file spn) (view spn_sline spn) (view spn_scol spn)

levelFixity :: Fixity' -> PrecLevel -> [OperatorRepr]
levelFixity fix = maybe [] id . Map.lookup fix

mapTryChoice :: (Foldable f, Functor f, MonadParsec s m t) => f a -> (a -> m b)-> m b
mapTryChoice ops act = choice (fmap (try . act) ops)

type ConSnoc f a = (Alternative f, Cons (f a) (f a) a a, Snoc (f a) (f a) a a)
listSpan :: ConSnoc f ExprSpan => f ExprSpan -> SrcSpan
listSpan xs = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)

precTable :: OpParser PrecTable
precTable = gets (buildTable . sort . fmap snd . ST.toList)
    where
        sort :: [Operator] -> [Operator]
        sort = sortBy (compare `on` previewOpPrec)

        previewOpPrec :: Operator -> Maybe Int
        previewOpPrec = preview (op_fix.fix_prec)

        buildTable :: [Operator] -> PrecTable
        buildTable = fst . foldr go ([Map.empty], Nothing)
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


instance Show a => ShowToken (Expression a) where
        showToken = prettyShow

instance Show a => ShowToken [Expression a] where
        showToken = concatMap prettyShow

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
                        setPosition (spanPos an)        -- probably this does nothing
                        x' <- generateOpParser

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

generateOpParser :: OpParser ExprSpan
generateOpParser = topP <* eof
    where
        closedPartP :: ConSnoc f ExprSpan => OperatorRepr -> OpParser (f ExprSpan)
        closedPartP = foldr go (pure empty) . closedPart
            where
                go :: Alternative f => OperatorPart -> OpParser (f ExprSpan) -> OpParser (f ExprSpan)
                go mpart act = partP mpart <|> act
                partP :: Alternative f => OperatorPart -> OpParser (f ExprSpan)
                partP mprt = case mprt of
                        Just prt -> var prt *> pure empty
                        Nothing  -> pure <$> topP
                closedPart :: OperatorRepr -> OperatorRepr
                closedPart = reverse . clean . reverse . clean
                    where
                        clean :: [Maybe String] -> [Maybe String]
                        clean = dropWhile isNothing

        topP :: OpParser ExprSpan
        topP = precTable >>= choice . foldr go [bottomP]
            where
                -- keep every parser level, passing them as the *hole*
                -- parser to use in evey upper level
                go :: PrecLevel -> [OpParser ExprSpan] -> [OpParser ExprSpan]
                go lvl acts = try (levelP lvl acts) : acts

        bottomP :: OpParser ExprSpan
        bottomP = do
                xs <- fromList <$> (some basicToken)
                return (Apply xs (listSpan xs))
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
                                        opParts = error "opParts: use prec variable"

                        closedfixP :: OpParser ExprSpan
                        closedfixP = do
                                closedOps <- concatMap (levelFixity (Closedfix ())) <$> precTable
                                mapTryChoice closedOps $ \op -> do
                                        xs <- closedPartP op
                                        let spn = listSpan xs
                                            nam = opStr op
                                        return (Apply (Var nam spn <| xs) spn)

        levelP :: PrecLevel -> [OpParser ExprSpan] -> OpParser ExprSpan
        levelP lvl below = try nonAssocP <|> try rightP <|> leftP
            where
                belowP :: OpParser ExprSpan
                belowP = choice below

                infixOps :: Associativity -> [OperatorRepr]
                infixOps ass = levelFixity (Infix ass dummy ()) lvl
                    where
                        dummy = error "infixOps: shouldn't be evaluated"

                genericOpP :: ConSnoc f ExprSpan
                           => [OperatorRepr]    -- the operators to use
                           -> Bool              -- if there is a left hand side
                           -> Bool              -- if tehre is a right hand side
                           -> OpParser (ExprSpan, f ExprSpan)
                genericOpP list leftHS rightHS = mapTryChoice list $ \op -> do
                        leftx  <- if leftHS  then pure <$> belowP else empty
                        clxs   <- closedPartP op
                        rightx <- if rightHS then pure <$> belowP else empty

                        let xs  = leftx <|> clxs <|> rightx
                            spn = listSpan xs
                            nam = opStr op

                        return (Var nam spn, xs)

                nonAssocP :: OpParser ExprSpan
                nonAssocP = do
                        (op, xs) <- genericOpP (infixOps NonAssoc) True True
                        return (Apply (op <| xs) (listSpan xs))
                -- nonAssocP :: OpParser ExprSpan
                -- nonAssocP = mapTryChoice (infixOps NonAssoc) $ \op -> do
                --         leftHS  <- belowP
                --         clxs    <- closedPartP op
                --         rightHS <- belowP
                --         let spn = (srcSpanSpan `on` (view exp_annot)) leftHS rightHS
                --             nam = opStr op
                --             xs  = leftHS <| (clxs |> rightHS)
                --         return (Apply (Var nam spn <| xs) spn)

                rightP :: OpParser ExprSpan
                rightP = do
                        xs <- some (try prefixP <|> rightAssocP)
                        x  <- belowP
                        return (foldr go x xs)
                    where
                        go :: (ExprSpan, Seq ExprSpan) -> ExprSpan -> ExprSpan
                        go (op, xs) x = let xs' = xs |> x
                                        in Apply (op <| xs') (listSpan xs')

                        prefixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        prefixP = genericOpP prefixOps False False
                            where
                                prefixOps :: [OperatorRepr]
                                prefixOps = levelFixity (Prefix dummy ()) lvl
                                    where
                                        dummy = error "prefixOps: shouldn't be evaluated"
                        -- prefixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        -- prefixP = mapTryChoice prefixOps $ \op -> do
                        --         xs <- closedPartP op
                        --         let spn = listSpan xs
                        --             nam = opStr op
                        --         return (Var nam spn, xs)

                        rightAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        rightAssocP = genericOpP (infixOps RightAssoc) True False
                        -- rightAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        -- rightAssocP = mapTryChoice (infixOps RightAssoc) $ \op -> do
                        --         leftHS <- belowP
                        --         clxs   <- closedPartP op
                        --         let xs  = leftHS <| clxs
                        --             spn = listSpan xs
                        --             nam = opStr op
                        --         return (Var nam spn, xs)

                leftP :: OpParser ExprSpan
                leftP = do
                        x  <- belowP
                        xs <- some (try postfixP <|> leftAssocP)
                        return (foldl go x xs)
                    where
                        go :: ExprSpan -> (ExprSpan, Seq ExprSpan) -> ExprSpan
                        go x (op, xs) = let xs' = x <| xs
                                        in Apply (op <| xs') (listSpan xs')

                        postfixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        postfixP = genericOpP postfixOps False False
                            where
                                postfixOps :: [OperatorRepr]
                                postfixOps = levelFixity (Postfix dummy ()) lvl
                                    where
                                        dummy = error "postfixOps: shouldn't be evaluated"
                        -- postfixP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        -- postfixP = mapTryChoice postfixOps $ \op -> do
                        --         xs <- closedPartP op
                        --         let spn = listSpan xs
                        --             nam = opStr op
                        --         return (Var nam spn, xs)

                        leftAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        leftAssocP = genericOpP (infixOps LeftAssoc) False True
                        -- leftAssocP :: ConSnoc f ExprSpan => OpParser (ExprSpan, f ExprSpan)
                        -- leftAssocP = mapTryChoice (infixOps LeftAssoc) $ \op -> do
                        --         clxs    <- closedPartP op
                        --         rightHS <- belowP
                        --         let xs  = clxs |> rightHS
                        --             spn = listSpan xs
                        --             nam = opStr op
                        --         return (Var nam spn, xs)



mixfixArgument = undefined
mixfixBody = undefined
mixfixExpression = undefined
mixfixImplicit = undefined
mixfixTypeBind = undefined
