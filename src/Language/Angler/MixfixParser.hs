{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Angler.MixfixParser where

import           Language.Angler.AST
import           Language.Angler.SymbolTable (SymbolTable)
import           Language.Angler.SrcLoc

import           Control.Applicative         (Alternative(..), (<|>), (<*>),
                                              (<*), (*>), liftA, many, some)
import           Control.Lens                hiding (op, below, parts)
import           Control.Monad.State         (State, runState)

import           Data.Map.Strict             (Map, lookup)
import           Data.Sequence               (Seq, fromList)

import           Text.Megaparsec             (ParsecT(..), choice, eof, runParserT, token, try)
import           Text.Megaparsec.Pos         (SourcePos(..), setSourceName,
                                              setSourceLine, setSourceColumn)
import           Text.Megaparsec.ShowToken   (ShowToken(..))
import           Text.Megaparsec.Error       (ParseError, Message(..))

import           Prelude                     hiding (lookup)

--------------------------------------------------------------------------------
-- Operator handling

type OperatorPart    = Maybe String                     -- Nothing represents hole
type Operator        = [ OperatorPart ]
type PrecedenceLevel = Map (Fixity ()) [ Operator ]
type Precedences     = [ PrecedenceLevel ]

opStr :: Operator -> String
opStr = concatMap (maybe "_" id)

strOp :: String -> Operator
strOp = foldr go []
    where
        go :: Char -> Operator -> Operator
        go c ps = case (c,ps) of
                ('_', _           ) -> Nothing    : ps
                ( _ , Just p : ps') -> Just (c:p) : ps'
                ( _ , _           ) -> Just [c]   : ps

--------------------------------------------------------------------------------
-- State

data OpPState
  = OpPState
        { _op_prec      :: Precedences
        , _op_table     :: SymbolTable ()
        }

makeLenses ''OpPState

--------------------------------------------------------------------------------
-- Monad

type Mixfix = State OpPState

runMixfix :: Mixfix a -> OpPState -> (a, OpPState)
runMixfix = runState

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

satisfy :: (ExprSpan -> Bool) -> OpP ExprSpan
satisfy g = token nextPos testExpr
    where
        testExpr :: ExprSpan -> Either [Message] ExprSpan
        testExpr x = if g x
                then Right x
                else (Left . return . Unexpected . showToken) x
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

var :: String -> OpP ExprSpan
var str = satisfy testExpr
    where
        testExpr :: ExprSpan -> Bool
        testExpr x = case x of
                Var name _ -> name == str
                _          -> False

generateOpP :: OpP ExprSpan
generateOpP = topOpP <* eof
    where
        topOpP :: OpP ExprSpan
        topOpP = use op_prec >>= choice . foldr go [bottomOpP] . fmap pOpP
            where
                -- We pass all the parsers below as *fallback*
                -- if we couldn't get a match on this level
                go :: ([OpP ExprSpan] -> OpP ExprSpan) -> [OpP ExprSpan] -> [OpP ExprSpan]
                go pp acts = try (pp acts) : acts

        bottomOpP :: OpP ExprSpan
        bottomOpP = do
                xs <- liftA fromList cleverTokens
                let xsSpan = srcSpanSpan (xs^?!_head.exp_annot) (xs^?!_last.exp_annot)
                return (Apply xs xsSpan)
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
                basicToken = try obviousToken <|> closedOpOpP
                    where
                        obviousToken :: OpP ExprSpan
                        obviousToken = use op_prec >>= satisfy . testExpr
                            where
                                testExpr :: Precedences -> ExprSpan -> Bool
                                testExpr prec x = case x of
                                        Var name _ -> name `notElem` (parts prec)
                                        _          -> True
                                parts :: Precedences -> [String]
                                parts = toListOf (traverse.traverse.traverse.traverse._Just)

                closedOpOpP :: OpP ExprSpan
                closedOpOpP = use op_prec >>= choice . fmap (try . opOpP) . closedOps
                    where
                        opOpP :: Operator -> OpP ExprSpan
                        opOpP op = do
                                clxs <- closedPartOpP op
                                let clsp = srcSpanSpan (clxs^?!_head.exp_annot) (clxs^?!_last.exp_annot)
                                return (Apply (Var (opStr op) clsp <| clxs) clsp)
                        closedOps :: Precedences -> [Operator]
                        closedOps = concatMap (maybe [] id . lookup (Closedfix ()))

        pOpP :: PrecedenceLevel -> [OpP ExprSpan] -> OpP ExprSpan
        pOpP _lvl below = choice below

        closedPartOpP :: Alternative f => Operator -> OpP (f ExprSpan)
        closedPartOpP = foldr go (pure empty)
            where
                go :: Alternative f => OperatorPart -> OpP (f ExprSpan) -> OpP (f ExprSpan)
                go mprt act = (<|>) <$> prtOpP mprt <*> act
                prtOpP :: Alternative f => OperatorPart -> OpP (f ExprSpan)
                prtOpP mprt = case mprt of
                        Just prt -> pure empty <* var prt
                        _        -> pure <$> topOpP
