{-# OPTIONS_GHC -w #-}
module Rewrite where

import Data.Foldable

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*>), liftA, liftA2)

import Prelude hiding (lookup)

data Expr
  = Apply [Expr]
  | Id String
  deriving (Eq, Show)

newtype ExprS = S Expr

toS :: [Expr] -> ExprS
toS es = S (Apply es)

instance Show ExprS where
        show = showS 0

showS :: Int -> ExprS -> String
showS n (S e) = showE n e

showE :: Int -> Expr -> String
showE n e = case e of
        Id s -> indent n ++ "«" ++ s ++ "»"
        Apply es -> concatMap (\e' -> "\n" ++ showE (n+1) e') es
    where
        indent n' = concat $ replicate n' "    "

data Operator
  = Op String
  | Hole
  deriving Show

data Assoc = ANon | ALeft | ARight

data Fixity = Pre | Post | Inf Assoc

-- from strong to weak
ops :: [ ( Fixity, [String] ) ]
ops = [ --(Post      , ["!_"]           )
        (Inf ALeft , ["_+_", "_-_"]   )
      , (Inf ANon  , ["_==_"]         )
      , (Inf ARight, ["_/\\_"]        )
      , (Pre       , ["if_then_else_"])
      ]

toExprs :: String -> [Expr]
toExprs = fmap Id . words

t0 = toExprs "if f a then 2 else if b then 3 else 4"
t1 = toExprs "a /\\ b c /\\ d e f /\\ g"
t2 = toExprs "if a /\\ b then c else d /\\ e f"
t3 = toExprs "a == b /\\ c d == e f"
t4 = toExprs "a == " ++ [Apply $ toExprs "if b c then d /\\ if e then f else g else h == i"]
t5 = toExprs "a + b - c + d"

t6 = toExprs "if a + b == c - d + e - f g /\\ h == i then j else k"
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

{-
        Expr -> P... | closed+
        P    -> OPcl
              | P'  OPnon P'
              | Pr+ P'
              | P'  Pl+
        Pr   -> OPprefix
              | P' OPright
        Pl   -> OPpostfix
              | OPleft P'
        P'   -> Smaller P... | closed+

        OP   -> {- parts of the operator -}

        closed -> identifier - OPsParts | ( Expr ) | closedOPs

-}

type PExpr  = Parsec [Expr] () Expr
type PNExpr = Parsec [Expr] () NExpr

satisfy :: (Expr -> Bool) -> PExpr
satisfy g = tokenPrim show nextPos testTok
    where
        nextPos p _t _ts = p
        testTok t = if g t then Just t else Nothing

idn :: String -> PExpr
idn str = satisfy testTok
    where
        testTok t = case t of
                Id i | i == str -> True
                _               -> False

term :: PExpr
term = satisfy (const True)

expr :: PExpr
expr = try pif <|> try pand <|> try peq <|> try psum <|> try closed1 <?> "expression"

closed :: PExpr
closed = satisfy testTok
    where
        testTok t = case t of
                Id i | i `elem` ["if", "then", "else", "/\\", "==", "+", "-"]
                   -> False
                _  -> True

closed1 :: PExpr
closed1 = many1 closed >>= return . flatten . Apply
    where
        flatten x = case x of
                Apply [o] -> o
                _         -> x

pif :: PExpr
pif = try prif
    where
        prif = do
                ps <- many1 prif'
                p <- p'
                return $ foldr (\xs x -> Apply (xs ++ [x]) ) p ps
        prif' = try opprefif
        opprefif = do
                i1 <- idn "if"
                e1 <- expr
                i2 <- idn "then"
                e2 <- expr
                i3 <- idn "else"
                return [Id "if_then_else_", e1, e2]
        p' = try pand <|> try peq <|> try psum <|> try closed1

pand :: PExpr
pand = try prand
    where
        prand = do
                ps <- many1 prand'
                p <- p'
                return $ foldr (\xs x -> Apply (xs ++ [x]) ) p ps
        prand' = try $ do
                p <- p'
                op : ps <- oprightand
                return $ op : ps ++ [p]
        oprightand = idn "/\\" >> return [Id "_/\\_"]
        p' = try peq <|> try psum <|> try closed1

peq :: PExpr
peq = try $ do
        l <- p'
        op : ps <- opnoneq
        r <- p'
        return $ Apply (op : [l] ++ ps ++ [r] )
    where
        opnoneq = idn "==" >> return [Id "_==_"]
        p' = try psum <|> try closed1

psum :: PExpr
psum = try $ do
        p <- p'
        ps <- many1 plsum'
        return $ foldl (\x (op:xs) -> Apply (op : x : xs) ) p ps
    where
        plsum' = try $ do
                op:ps <- opleftsum
                p <- p'
                return $ op : p : ps
        opleftsum = try (idn "+" >> return [Id "_+_"])
                <|> try (idn "-" >> return [Id "_-_"])
        p' = try closed1
