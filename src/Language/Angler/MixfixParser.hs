{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Rewrite where

import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, fromList, empty, lookup, insertWith)

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*>), (<*), liftA, liftA2)
import Control.Lens hiding (op)

import Prelude hiding (lookup)

import Debug.Trace

data Expr
  = Apply [Expr]
  | Id String
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

ops' :: [ PrecedenceLevel ]
ops' = map ($ empty)
        [ add (Infix ANon) [strOp "_<_>_"]
        , add (Infix ANon) [strOp "_==_"]
        , add  Closed      [strOp "<<_>>"]
        , add  Closed      [strOp "(^_^)"]
        , add (Infix ALeft) [strOp "_<>_"] . add (Infix ARight) [strOp "_|_"]
        ]
    where
        add = insertWith (++)

-- from weak to strong
ops :: [ PrecedenceLevel ]
ops = map ($ empty)
        [ add  Pre            [strOp "if_then_else_"]
        , add (Infix ARight)  [strOp "_/\\_"]
        , add (Infix ANon)    [strOp "_==_"]
        , add (Infix ALeft)   [strOp "_+_", strOp "_-_"]
        , add  Post           [strOp "_!"]
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
    Apply [x] -> flattenExpr x
    Apply xs  -> Apply (map flattenExpr xs)
    _         -> ex


unApply :: Expr -> [Expr]
unApply ex = case ex of
    Apply xs  -> xs
    otherwise -> [ex]

t0 = unApply $ toExpr "if f a then 2 else if b then 3 else 4"
t1 = unApply $ toExpr "a /\\ b c /\\ d e f /\\ g"
t2 = unApply $ toExpr "if a /\\ b then c else d /\\ e f"
t3 = unApply $ toExpr "a == b /\\ c d == e f"
t4 = unApply $ toExpr "a == ( if b c then d /\\ if e then f else g else h == i )"
t5 = unApply $ toExpr "a + b - c + d"
t7 = unApply $ toExpr "a == b + c /\\ d == e"

t6 = unApply $ toExpr "if a + b == c - d + e - f g /\\ h == i then j else k"
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

type P a     = Parsec [Expr] () a

satisfy :: (Expr -> Bool) -> P Expr
satisfy g = tokenPrim show nextPos testTok
    where
        testTok t = if g t then Just t else Nothing
        nextPos p t _ts = case t of
                Id str -> setSourceColumn p (sourceColumn p + length str)
                Apply xs -> setSourceColumn p (sourceColumn p + inLength xs)
            where
                inLength :: [Expr] -> Int
                inLength = sum . map check
                check :: Expr -> Int
                check x = case x of
                    Id str -> length str
                    Apply xs -> inLength xs

iden :: String -> P Expr
iden s = satisfy testTok
    where
        testTok t = case t of
            Id i -> i == s
            _    -> False

choiceTry :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
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

genn :: [ PrecedenceLevel ] -> P Expr
genn lvls = exprParser <* eof
    where

        exprParser :: P Expr
        exprParser = choiceTry (foldr go [bottomParser] (map pParser lvls))
            where
                go :: ([P Expr] -> P Expr) -> [P Expr] -> [P Expr]
                go pp acts = pp acts : acts

        bottomParser :: P Expr
        bottomParser = flattenExpr <$> Apply <$> (try (many1 basicBottom) <|> anyBottom)
            where
                -- This may or not be recommended
                anyBottom :: P [Expr]
                anyBottom = cons <$> anyToken <*> many basicBottom
                    where
                        anyToken = satisfy (const True)

                basicBottom :: P Expr
                basicBottom = try nonPartParser <|> try closedParser
                    where
                        nonPartParser :: P Expr
                        nonPartParser = tokenPrim show (\p _ _ -> p) testTok
                            where
                                testTok :: Expr -> Maybe Expr
                                testTok tk = case tk of
                                        Id str   -> if str `elem` opsParts then Nothing else Just tk
                                        Apply xs -> unEither (parse (genn lvls) "" xs)
                                        -- use getInput, updateState, setInput
                                    where
                                        opsParts :: [String]
                                        opsParts = concatMap operatorParts lvls
                                        unEither :: Either b a -> Maybe a
                                        unEither ei = case ei of
                                                Right x -> Just x
                                                _       -> Nothing

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
                middleParser = choiceTry $ flip map (nonops lvl) $ \op -> do
                        l    <- pParser'
                        clsd <- closedPartParser (cleanOp op)
                        r    <- pParser'
                        return (Apply $ [Id (opStr op)] ++ [l] ++ clsd ++ [r])

                rightParser :: P Expr
                rightParser = do
                        ps <- many1 (try prefixParser <|> try rightAssocParser)
                        p  <- pParser'
                        return $ foldr (\xs x -> Apply (xs ++ [x]) ) p ps
                    where
                        prefixParser :: P [Expr]
                        prefixParser = choiceTry $ flip map (prefixops lvl) $ \op -> do
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ clsd)

                        rightAssocParser :: P [Expr]
                        rightAssocParser = choiceTry $ flip map (rightassocops lvl) $ \op -> do
                                l    <- pParser'
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ [l] ++ clsd)

                leftParser :: P Expr
                leftParser = do
                        p  <- pParser'
                        ps <- many (try postfixParser <|> try leftAssocParser)
                        return $ foldl' (\x (op:xs) -> Apply (op : x : xs) ) p ps
                    where
                        postfixParser :: P [Expr]
                        postfixParser = choiceTry $ flip map (postfixops lvl) $ \op -> do
                                clsd <- closedPartParser (cleanOp op)
                                return ([Id (opStr op)] ++ clsd)

                        leftAssocParser :: P [Expr]
                        leftAssocParser = choiceTry $ flip map (leftassocops lvl) $ \op -> do
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
                                Just prt -> iden prt >> return []
                                _        -> (\x -> cons x []) <$> exprParser
