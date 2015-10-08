{-# LANGUAGE RankNTypes #-}

module PrettyShow
    ( PrettyShow(pshow), PrettyShowMonad
    , prettyShow, prettyShowIndent

    , pshows
    , string, lstring
    , line
    , raise, lower
    ) where

import           Control.Lens
import           Control.Monad.State     (State, execState)

import           Data.Foldable           (toList)

type Indentation = Int
type PrettyShowMonad = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_level     :: Indentation
        , _ps_lines     :: [(Indentation, String)]
        }

makeLenses ''PrettyShowState

class PrettyShow a where
        pshow :: a -> PrettyShowMonad

prettyShow :: PrettyShow a => a -> String
prettyShow = prettyShowIndent 0 "    "

prettyShowIndent :: PrettyShow a
                 => Indentation         -- Start indentation level
                 -> String              -- Indentation string
                 -> a -> String
prettyShowIndent n str = showLines . view ps_lines . flip execState initialST . pshow
    where
        showLines :: [(Indentation, String)] -> String
        showLines = concatMap (\(ind, s) -> tabs ind ++ s ++ "\n") . reverse
        tabs :: Indentation -> String
        tabs ind = concat (replicate ind str)
        initialST :: PrettyShowState
        initialST = PrettyShowState
                { _ps_level = n
                , _ps_lines  = [(n, "")]
                }

----------------------------------------

pshows :: (PrettyShow a, Foldable f) => PrettyShowMonad -> f a -> PrettyShowMonad
pshows act xs = case toList xs of
        p : ps -> pshow p >> mapM_ (\x -> act >> pshow x) ps
        _      -> return ()

string :: String -> PrettyShowMonad
string str = ps_lines._head._2 %= (++ str)

lstring :: Lens' s String -> s -> PrettyShowMonad
lstring lns = string . view lns

line :: PrettyShowMonad
line = use ps_level >>= \n -> ps_lines %= cons (n, "")

raise :: PrettyShowMonad
raise = ps_level += 1

lower :: PrettyShowMonad
lower = ps_level -= 1
