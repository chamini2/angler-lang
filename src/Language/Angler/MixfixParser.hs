{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Rewrite where

import Data.Foldable
import Data.Maybe
import Data.Map.Strict (Map, fromList, empty, lookup, insertWith)

import Text.Parsec hiding (satisfy)

import Control.Applicative ((<*>), liftA, liftA2)
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

ops'' :: [ PrecedenceLevel ]
ops'' = map ($ empty)
        [ add  Pre         [strOp "-_"]
        , add (Infix ANon) [strOp "_=_"]
        , add  Closed      [strOp "[_]"]
        , add  Pre         [strOp "if_then_else_"]
        , add (Infix ANon) [strOp "_<_>_"]
        ]
    where
        add = insertWith (++)

ops' :: [ PrecedenceLevel ]
ops' = map ($ empty)
        [ add (Infix ANon) [strOp "_<_>_"]
        , add (Infix ANon) [strOp "_==_"]
        , add  Closed      [strOp "<<_>>"]
        , add  Closed      [strOp "(^_^)"]
        , add (Infix ANon) [strOp "_<>_"]
        ]
    where
        add = insertWith (++)

ops :: [ PrecedenceLevel ]
ops = map ($ empty)
        [ add  Post           [strOp "_!"]
        , add (Infix ALeft)   [strOp "_+_", strOp "_-_"]
        , add (Infix ANon)    [strOp "_==_"]
        , add (Infix ARight)  [strOp "_/\\_"]
        , add  Pre            [strOp "if_then_else_"]
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

type PExpr   = Parsec [Expr] () Expr
type PLExpr  = Parsec [Expr] () [Expr]
type POpExpr = Parsec [Expr] () (Either Expr Expr)

satisfy :: (Expr -> Bool) -> PExpr
satisfy g = tokenPrim show nextPos testTok
    where
        nextPos p _t _ts = p
        testTok t = if g t then Just t else Nothing

iden :: String -> PExpr
iden s = satisfy testTok
    where
        testTok t = case t of
            Id i | i == s -> True
            _             -> False

choiceTry :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choiceTry = choice . map try

-- type OpPart = Maybe String
-- type Operator = [OpPart]
-- type PrecedenceLevel = Map Fixity [ Operator ]

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

genn :: [ PrecedenceLevel ] -> PExpr
genn lvls = exprParser
    where

        exprParser :: PExpr
        exprParser = choiceTry parsers
            where
                parsers :: [PExpr]
                parsers = foldr go [bottomParser] (map pParser lvls)
                    where
                        go :: ([PExpr] -> PExpr) -> [PExpr] -> [PExpr]
                        go pp acts = pp acts : acts

        bottomParser :: PExpr
        bottomParser = flattenExpr <$> Apply <$> many1 (identifier <|> closedParser)
            where
                identifier :: PExpr
                identifier = satisfy testTk
                    where
                        testTk :: Expr -> Bool
                        testTk tk = case tk of
                                Id str -> str `notElem` opsParts
                                _      -> True
                        opsParts :: [String]
                        opsParts = concatMap operatorParts lvls

                closedParser :: PExpr
                closedParser = choiceTry (map parseOp ops)
                    where
                        parseOp :: Operator -> PExpr
                        parseOp op = do
                                clsd <- closedPartParser op
                                return (Apply $ [Id (opStr op)] ++ clsd)
                        ops :: [Operator]
                        ops = concatMap closedops lvls

        pParser :: PrecedenceLevel -> [PExpr] -> PExpr
        pParser lvl below = try middleParser
                        -- <|> try rightParser
                        -- <|> try leftParser
            where
                pParser' :: PExpr
                pParser' = choiceTry below

                middleParser :: PExpr
                middleParser = choiceTry $ flip map (nonops lvl) $ \op -> do
                        l    <- pParser'
                        clsd <- closedPartParser (cleanOp op)
                        r    <- pParser'
                        return (Apply $ [Id (opStr op)] ++ [l] ++ clsd ++ [r])
                        -- l    <- if nothingHead op then pure <$> pParser' else return []
                        -- clsd <- closedPartParser (cleanOp op)
                        -- r    <- if nothingLast op then pure <$> pParser' else return []
                        -- return (Apply $ [Id (opStr op)] ++ l ++ clsd ++ r)
                    where
                        cleanOp :: Operator -> Operator
                        cleanOp op = (if nothingHead op then tail else id) . (if nothingLast op then init else id) $ op
                        nothingHead :: Operator -> Bool
                        nothingHead = isNothing . head
                        nothingLast :: Operator -> Bool
                        nothingLast = isNothing . last

                rightParser :: PExpr
                rightParser = do
                        ps <- many1 rightParser'
                        p  <- pParser'
                        return $ foldr (\xs x -> Apply (xs ++ [x]) ) p ps
                    where
                        rightParser' :: PLExpr
                        rightParser' = try prefixParser -- <|> try rightAssocParser
                            where
                                prefixParser :: PLExpr
                                prefixParser = choiceTry $ flip map (prefixops lvl) $ \op -> do
                                        clsd <- closedPartParser (cleanOp (traceShowId op))
                                        return ([Id (opStr op)] ++ clsd)

                                rightAssocParser :: PLExpr
                                rightAssocParser = undefined

                        cleanOp :: Operator -> Operator
                        cleanOp op = (if nothingHead op then tail else id) . (if nothingLast op then init else id) $ op
                        nothingHead :: Operator -> Bool
                        nothingHead = isNothing . head
                        nothingLast :: Operator -> Bool
                        nothingLast = isNothing . last


                leftParser :: PExpr
                leftParser = undefined

        closedPartParser :: [OpPart] -> PLExpr
        closedPartParser = foldr go (return [])
            where
                go :: OpPart -> PLExpr -> PLExpr
                go mprt act = (++) <$> lst mprt <*> act
                    where
                        lst :: OpPart -> PLExpr
                        lst mprt = case mprt of
                                Just prt -> iden prt >> return []
                                _        -> pure <$> exprParser

{-
genPar :: [PrecedenceLevel] -> PExpr
genPar lvls = exprparser
    where

        exprparser :: PExpr
        exprparser = choiceTry (map ($ exprparser) undefined {-rest-})

        rest :: [PExpr -> PExpr]
        rest = foldr go [bottomparser] (map pparser lvls)

        bottomparser :: PExpr
        bottomparser expr = flatten <$> Apply <$> many1 (try identifier <|> try closedparser)
            where
                identifier :: PExpr
                identifier = getState >>= satisfy . testTk
                    where
                        testTk prts tk = case tk of
                            Id s | s `elem` prts -> False
                            _                    -> True

                closedparser :: PExpr
                closedparser = iden "(" >> expr >>= \x -> iden ")" >> return (Apply [Id "(_)", x])

                flatten :: Expr -> Expr
                flatten ex = case ex of
                        Apply [x] -> x
                        _         -> ex

        go :: ([PExpr -> PExpr] -> PExpr -> PExpr) -> [PExpr -> PExpr] -> [PExpr -> PExpr]
        go pp acts = pp acts : acts

        pparser :: PrecedenceLevel -> [PExpr -> PExpr] -> PExpr -> PExpr
        pparser fxops below expr = try nonparser
                               -- <|> try rightparser
                               -- <|> try leftparser
            where
                pparser' :: PExpr
                -- pparser' = choicet (map (\f -> f expr) below)
                pparser' = choicet (map ($ expr) below)

                guardops :: Fixity -> [(String, Operator)]
                guardops fx = map (\(_,n,p) -> (n,p)) (filter ((==fx) . view _1) fxops)
                infops :: Assoc -> [(String, Operator)]
                infops = guardops . Infix

                nonparser :: PExpr
                nonparser = if null nonops
                        then fail "no AssocNon operators"
                        else do
                            l      <- pparser'
                            o : ps <- nonopsparser
                            r      <- pparser'
                            return (Apply (o : l : ps ++ [r]))
                    where
                        nonops :: [(String, Operator)]
                        nonops = infops ANon
                        nonopsparser :: PLExpr
                        nonopsparser = makeClosedPart nonops

                rightparser :: PExpr
                rightparser = undefined

                leftparser :: PExpr
                leftparser = undefined

                makeClosedPart :: [(String, Operator)] -> PLExpr
                makeClosedPart = choicet . over traverse treat'
                    where
                        treat' :: (String, [OpPart]) -> PLExpr
                        treat' (opnm, prts) = cons (Id opnm) <$> foldr go (fail "end of treat") prts
                            where
                                go :: OpPart -> PLExpr -> PLExpr
                                go mprt act = (++) <$> lst mprt <*> act
                                lst :: OpPart -> PLExpr
                                lst = maybe ((: []) <$> expr) ((const [] <$>) . iden)

-}

{-generateParser :: [ PrecedenceLevel ] -> PExpr
generateParser fxs = expr
    where
        expr :: PExpr
        expr = head (foldl go [closed (iden "a")] fxs)
            where
                go :: [PExpr] -> PrecedenceLevel -> [PExpr]
                go ps fops = p : ps
                    where
                        p :: PExpr
                        p = try opclosed <|> try opnon -- <|> try opright <|> try opleft

                        p' :: PExpr
                        p' = choice ps

                        guardops :: Fixity -> [(String, Operator)]
                        guardops fx = map (\(_,n,p) -> (n,p)) (filter ((==fx) . view _1) fops)
                        infops :: Assoc -> [(String, Operator)]
                        infops = guardops . Infix

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
-}
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
