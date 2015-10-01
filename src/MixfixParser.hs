{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Rewrite where

import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, fromList, empty, lookup, insertWith)

import Text.Megaparsec hiding (satisfy)
import Text.Megaparsec.Pos (incSourceColumn)
import Text.Megaparsec.ShowToken (ShowToken(..))

import Control.Applicative ((<*>), (<*), liftA, liftA2)
import Control.Lens hiding (op)

import Prelude hiding (lookup)

import Debug.Trace

data Expr
  = Apply [Expr]
  | Id String
  | Forall String Expr Expr
  deriving (Eq, Show)

type OpPart = Maybe String
type Operator = [OpPart]

data Assoc
  = ANon
  | ALeft
  | ARight
  deriving (Eq, Show, Ord)
makePrisms ''Assoc

data Fixity
  = Pre
  | Post
  | Infix Assoc
  | Closed
  deriving (Eq, Show, Ord)
makePrisms ''Fixity

opStr :: Operator -> String
opStr = concatMap (maybe "_" id)

strOp :: String -> Operator
strOp = foldr go []
    where
        go c prts = case (c, prts) of
                ('_', Nothing : ps) -> error "two holes in a row in an identifier"
                ('_', _           ) -> Nothing    : prts
                ( _ , Just p  : ps) -> Just (c:p) : ps
                ( _ , _           ) -> Just [c]   : prts

type PrecedenceLevel = Map Fixity [ Operator ]
type OperatorsPrecedence = [ PrecedenceLevel ]

ops' :: OperatorsPrecedence
ops' = map ($ empty)
        [ add (Infix ANon) [strOp "_<_>_"]
        , add (Infix ALeft) [strOp "_<>_"] . add (Infix ARight) [strOp "_|_"]
        , add (Infix ANon) [strOp "_==_"]
        , add  Closed      [strOp "<<_>>"]
        , add  Closed      [strOp "(^_^)"]
        ]
    where
        add = insertWith (++)

-- from weak to strong
ops :: OperatorsPrecedence
ops = map ($ empty)
        [ add  Pre            [strOp "if_then_else_"]
        , add (Infix ARight)  [strOp "_/\\_"]
        , add (Infix ANon)    [strOp "_==_", strOp "_<_", strOp "_>_", strOp "_<=_", strOp "_>=_"]
        , add (Infix ALeft)   [strOp "_+_", strOp "_-_"]
        , add  Post           [strOp "_!"]
        , add  Closed         [strOp "<_,_>"]
        ]
    where
        add = insertWith (++)

toExpr :: String -> Expr
toExpr = flattenExpr . Apply . fst . func . words
    where
        func wrds = case wrds of
            w : ws -> case w of
                    "(" -> let (xs', left') = func left in (Apply xs : xs', left')
                    ")" -> ([], ws)
                    _   -> (Id w : xs, left)
                where
                    (xs, left) = func ws
            _      -> ([], [])

flattenExpr :: Expr -> Expr
flattenExpr ex = case ex of
    Apply [x]    -> flattenExpr x
    Apply xs     -> Apply (map flattenExpr xs)
    Forall s t x -> Forall s (flattenExpr t) (flattenExpr x)
    Id x         -> Id x

t0 = toExpr "if f a then 2 else if b then 3 else 4"
t1 = toExpr "a /\\ b c /\\ d e f /\\ g"
t2 = toExpr "if a /\\ b then c else d /\\ e f"
t3 = toExpr "a == b /\\ c d == e f"
t4 = toExpr "a == ( if b c then d /\\ if e then f else g else h == i )"
t5 = toExpr "a + b - c + d"
t7 = toExpr "a == b + c /\\ d == e"

ops8 = [ insertWith (++) (Infix ARight) [strOp "_->_"] empty ]
t8 = Apply [Forall "t" (Id "Type") (Forall "n" (Id "Nat")
        (Apply [ Apply [Id "t", Id "->", Id "Bool"] , Id "->", Id "Vect"
               , Id "n", Id "t" , Id "->", Forall "m" (Id "Nat")
                        (Apply [Id "m", Id "->", Id "t"]) ] ) )]

t6 = toExpr "if a + b == c - d + e ! - f g /\\ h == i then j k else if l then m else n"
t6' = Right $ Apply
        [Id "if_then_else_",Apply
                [Id "_/\\_",Apply
                        [Id "_==_",Apply
                                [Id "_+_",
                                        Id "a",
                                        Id "b"
                                ],Apply
                                [Id "_-_",Apply
                                        [Id "_+_",Apply
                                                [Id "_-_",
                                                        Id "c",
                                                        Id "d"
                                                ],
                                                Id "e"
                                        ],Apply
                                        [Id "f",
                                                Id "g"
                                        ]
                                ]
                        ],Apply
                        [Id "_==_",
                                Id "h",
                                Id "i"
                        ]
                ],
                Id "j",
                Id "k"
        ]

--------------------------------------------------------------------------------

type P a = Parsec [Expr] a

instance ShowToken Expr where
        showToken = show

instance ShowToken a => ShowToken [a] where
        showToken = show

satisfy :: (Expr -> Bool) -> P Expr
satisfy g = token nextPos testTok
    where
        testTok :: Expr -> Either [Message] Expr
        testTok t = if g t
                then Right t
                else Left . pure . Unexpected . showToken $ t
        nextPos :: Int -> SourcePos -> Expr -> SourcePos
        nextPos _tb p t = incSourceColumn p (getLength t)
            where
                getLength :: Expr -> Int
                getLength x = case x of
                    Id str -> length str
                    Apply xs -> sum . map getLength $ xs
                    Forall str t x -> length str + getLength t + getLength x

iden :: String -> P Expr
iden s = satisfy testTok
    where
        testTok t = case t of
            Id i -> i == s
            _    -> False

choiceTry :: Stream s t => [ParsecT s m a] -> ParsecT s m a
choiceTry = choice . map try

operatorParts :: PrecedenceLevel -> [String]
operatorParts = toListOf (traverse.traverse.traverse._Just)

infops :: Assoc -> PrecedenceLevel -> [Operator]
infops ass = maybe [] id . lookup (Infix ass)

rightassocops :: PrecedenceLevel -> [Operator]
rightassocops = infops ARight

prefixops :: PrecedenceLevel -> [Operator]
prefixops = maybe [] id . lookup Pre

leftassocops :: PrecedenceLevel -> [Operator]
leftassocops = infops ALeft

postfixops :: PrecedenceLevel -> [Operator]
postfixops = maybe [] id . lookup Post

nonops :: PrecedenceLevel -> [Operator]
nonops = infops ANon

closedops :: PrecedenceLevel -> [Operator]
closedops = maybe [] id . lookup Closed

parseMixfix :: OperatorsPrecedence -> Expr -> Either ParseError Expr
parseMixfix prc ex = case ex of
        Id str   -> return (Id str)
        Apply xs -> let xsEits = fmap (parseMixfix prc) xs
                    in case find isLeft xsEits of
                        Just lf -> lf
                        Nothing -> precParser (toListOf (traverse._Right) xsEits)
        Forall str typ x -> case (parseMixfix prc typ, parseMixfix prc x) of
                (Right t', Right x') -> Right (Forall str t' x')
                (Left err, _       ) -> Left err
                (_       , Left err) -> Left err
    where
        precParser :: [Expr] -> Either ParseError Expr
        precParser = parse (genn prc) ""

genn :: OperatorsPrecedence -> P Expr
genn lvls = exprParser <* eof
    where

        exprParser :: P Expr
        exprParser = choiceTry (foldr go [bottomParser] (map pParser lvls))
            where
                go :: ([P Expr] -> P Expr) -> [P Expr] -> [P Expr]
                go pp acts = pp acts : acts

        bottomParser :: P Expr
        bottomParser = flattenExpr . Apply <$> some basicToken
        -- bottomParser = flattenExpr . Apply <$> tokens
            where
                -- This gets any token, and then continues with the basic tokens
                -- in 'if if a then b else c' it parses 'if_then_else_ (if a) b c'
                -- instead of giving a parse error.This behaviour may or
                -- may not be recommended.
                tokens :: P [Expr]
                tokens = cons <$> (try basicToken <|> anyToken) <*> many basicToken

                anyToken :: P Expr
                anyToken = satisfy (const True)

                basicToken :: P Expr
                basicToken = try nonPartParser <|> try closedParser
                    where
                        nonPartParser :: P Expr
                        nonPartParser = satisfy testTk
                            where
                                testTk :: Expr -> Bool
                                testTk tk = case tk of
                                        Id str -> str `notElem` opsParts
                                        _      -> True
                                opsParts :: [String]
                                opsParts = concatMap operatorParts lvls

                        closedParser :: P Expr
                        closedParser = choiceTry (map parseOp ops)
                            where
                                parseOp :: Operator -> P Expr
                                parseOp op = do
                                        clsd <- closedPartParser op
                                        return (Apply $ [Id (opStr op)] ++ clsd)
                                ops :: [Operator]
                                ops = concatMap closedops lvls

        pParser :: PrecedenceLevel -> [P Expr] -> P Expr
        pParser lvl below = try middleParser
                        <|> try rightParser
                        <|> try leftParser
            where
                pParser' :: P Expr
                pParser' = choiceTry below

                cleanOp :: Operator -> Operator
                cleanOp op = (if isNothing (head op) then tail else id) . (if isNothing (last op) then init else id) $ op

                middleParser :: P Expr
                middleParser = choiceTry . flip map (nonops lvl) $ \op -> do
                        l    <- pParser'
                        clsd <- closedPartParser (cleanOp op)
                        r    <- pParser'
                        return (Apply $ [Id (opStr op)] ++ [l] ++ clsd ++ [r])

                rightParser :: P Expr
                rightParser = do
                        ps <- some (try prefixParser <|> try rightAssocParser)
                        p  <- pParser'
                        return (foldr (\xs x -> Apply (xs ++ [x]) ) p ps)
                    where
                        prefixParser :: P [Expr]
                        prefixParser = choiceTry . flip map (prefixops lvl) $ \op -> do
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ clsd)

                        rightAssocParser :: P [Expr]
                        rightAssocParser = choiceTry . flip map (rightassocops lvl) $ \op -> do
                                l    <- pParser'
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ [l] ++ clsd)

                leftParser :: P Expr
                leftParser = do
                        p  <- pParser'
                        ps <- many (try postfixParser <|> try leftAssocParser)
                        return (foldl' (\x (op:xs) -> Apply (op : x : xs) ) p ps)
                    where
                        postfixParser :: P [Expr]
                        postfixParser = choiceTry . flip map (postfixops lvl) $ \op -> do
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ clsd)

                        leftAssocParser :: P [Expr]
                        leftAssocParser = choiceTry . flip map (leftassocops lvl) $ \op -> do
                                clsd <- closedPartParser (cleanOp op)
                                r    <- pParser'
                                return ([Id (opStr op)] ++ clsd ++ [r])


        closedPartParser :: [OpPart] -> P [Expr]
        closedPartParser = foldr go (return [])
            where
                go :: OpPart -> P [Expr] -> P [Expr]
                go mprt act = (++) <$> lst mprt <*> act
                    where
                        lst :: OpPart -> P [Expr]
                        lst mprt = case mprt of
                                Just prt -> iden prt *> return []
                                _        -> liftA (flip cons []) exprParser
