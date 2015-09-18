{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Rewrite where

import Data.Foldable
import Data.Maybe

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*>), liftA, liftA2)
import Control.Lens hiding (op)

import Prelude hiding (lookup)

data Expr
  = Apply [Expr]
  | Id String
  deriving (Eq, Show)

type OpPart   = Maybe String
type Operator = [OpPart]

data Assoc
  = ANon
  | ALeft
  | ARight
  deriving (Eq, Show)

data Fixity
  = Pre
  | Post
  | Inf Assoc
  | Closed
  deriving (Eq, Show)

makePrisms ''Assoc
makePrisms ''Fixity

type PrecedenceLevel = [ (Fixity, String, Operator) ]

-- from strong to weak
ops :: [ PrecedenceLevel ]
ops = [ --(Post      , ["!_"]           )
        [
          (Inf ALeft , "_+_"          , [Nothing, Just "+", Nothing])
        , (Inf ALeft , "_-_"          , [Nothing, Just "-", Nothing])
        ]
      , [ (Inf ANon  , "_==_"         , [Nothing, Just "==", Nothing])  ]
      , [ (Inf ARight, "_/\\_"        , [Nothing, Just "/\\", Nothing]) ]
      , [ (Pre       , "if_then_else_", [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]) ]
      ]

toExprs :: String -> [Expr]
toExprs = fmap Id . words

t0 = toExprs "if f a then 2 else if b then 3 else 4"
t1 = toExprs "a /\\ b c /\\ d e f /\\ g"
t2 = toExprs "if a /\\ b then c else d /\\ e f"
t3 = toExprs "a == b /\\ c d == e f"
t4 = toExprs "a == " ++ [Apply $ toExprs "if b c then d /\\ if e then f else g else h == i"]
t5 = toExprs "a + b - c + d"
t7 = toExprs "a == b + c /\\ d == e"

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

type PExpr    = Parsec [Expr] () Expr
type PLExpr   = Parsec [Expr] () [Expr]
type POpExpr  = Parsec [Expr] () (Either Expr Expr)

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

-- type OpPart   = Maybe String
-- type Operator = [OpPart]
-- type PrecedenceLevel = [ (Fixity, Operator) ]

generateParser :: [ PrecedenceLevel ] -> PExpr
generateParser fxs = expr
    where
        expr :: PExpr
        expr = head (foldr go [closed (idn "a")] fxs)
            where
                go :: PrecedenceLevel -> [PExpr] -> [PExpr]
                go fops ps = p : ps
                    where
                        p :: PExpr
                        p = try opclosed <|> try opnon -- <|> try opright <|> try opleft

                        p' :: PExpr
                        p' = choice ps

                        guardops :: Fixity -> [(String, Operator)]
                        guardops fx = map (\(_,n,p) -> (n,p)) (filter ((==fx) . view _1) fops)
                        infops :: Assoc -> [(String, Operator)]
                        infops = guardops . Inf

                        makeChoice :: PExpr -> [(String, Operator)] -> PLExpr
                        makeChoice expr = choice . map (treat . over _2 (map (maybe expr' iden')))
                            where
                                expr' :: POpExpr
                                expr' = Right <$> expr
                                iden' :: String -> POpExpr
                                iden' = (Left <$>) . iden

                                treat :: (String, [POpExpr]) -> PLExpr
                                treat (opnm, acts) = finish (foldr go (return []) acts)
                                    where
                                        finish :: PLExpr -> PLExpr
                                        finish act = (Id opnm :) <$> act
                                        go :: POpExpr -> PLExpr -> PLExpr
                                        go eact act = (++) <$> liftA toList eact <*> act
                                        -- go eact act = do
                                        --     eit <- eact
                                        --     xs <- act
                                        --     return (toList eit ++ xs)

                        opclosed :: PExpr
                        opclosed = Apply <$> makeChoice expr (guardops Closed)

                        opnon :: PExpr
                        opnon = do
                                l      <- p'
                                o : ps <- nonops
                                r      <- p'
                                return (Apply (o : l : ps ++ [r]))
                            where
                                nonops = makeChoice expr (infops ANon)

                        opright = undefined --do
                            --     ps <- many1 opright'
                            --     r  <- p'
                            -- where
                            --     opright' = do
                            --         l <- p'
-- pand :: PExpr
-- pand = try prand
--     where
--         prand = do
--                 ps <- many1 prand'
--                 p <- p'
--                 return $ foldr (\xs x -> Apply (xs ++ [x]) ) p ps
--         prand' = try $ do
--                 p <- p'
--                 op : ps <- oprightand
--                 return $ op : ps ++ [p]
--         oprightand = idn "/\\" >> return [Id "_/\\_"]
--         p' = try peq <|> try psum <|> closed1

                        opleft = undefined


                        rightops  = infops ARight
                        preops    = guardops Pre

                        leftops   = infops ALeft
                        postops   = guardops Post
        -- base parser
        closed :: PExpr -> PExpr
        closed expr = many1 (try nonOp <|> try (clsd clops)) >>= return . flatten . Apply
            where
                ops :: [Operator]
                ops = fxs^..traverse.traverse._3

                clops :: [Operator]
                clops = filter guard ops
                    where
                        guard op = isNothing (head op) && isNothing (last op)

                opnms :: [String]
                opnms = ops^..traverse.traverse._Just

                flatten :: Expr -> Expr
                flatten x = case x of
                    Apply [p] -> p
                    _         -> x

                nonOp :: PExpr
                nonOp = satisfy testTok
                    where
                        testTok t = case t of
                            Id i | i `elem` opnms -> False
                            _                     -> True

                clsd :: [Operator] -> PExpr
                clsd = choice . map (liftA Apply . sequence . map (maybe expr iden))

        iden :: String -> PExpr
        iden s = satisfy testTok
            where
                testTok t = case t of
                    Id i | i == s -> True
                    _             -> False

{-
term :: PExpr
term = satisfy (const True)

term1 :: PExpr
term1 = many1 term >>= return . flatten . Apply
    where
        flatten x = case x of
                Apply [o] -> o
                _         -> x

expr :: PExpr
expr = try pif <|> try pand <|> try peq <|> try psum <|> try closed1 <|> term1 <?> "expression"

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
        p' = try pand <|> try peq <|> try psum <|> closed1

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
        p' = try peq <|> try psum <|> closed1

peq :: PExpr
peq = try $ do
        l <- p'
        op : ps <- opnoneq
        r <- p'
        return $ Apply (op : [l] ++ ps ++ [r] )
    where
        opnoneq = idn "==" >> return [Id "_==_"]
        p' = try psum <|> closed1

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
        p' = closed1
-}
