{-# LANGUAGE RankNTypes #-}

module PrettyShow
    ( PrettyShow(pshow), PrettyShowed
    , prettyShow, prettyShowIndent, runPrettyShow, runPrettyShowIndent

    , pshows
    , string, lstring
    , line
    , raise, lower
    ) where

import           Control.Lens
import           Control.Monad.State     (State, execState)

import           Data.Foldable           (toList)
import           Data.List               (intercalate)

type Indentation = Int
type PrettyShowed = State PrettyShowState ()

data PrettyShowState
  = PrettyShowState
        { _ps_level     :: Indentation
        , _ps_lines     :: [(Indentation, String)]
        }

makeLenses ''PrettyShowState

class PrettyShow a where
        pshow :: a -> PrettyShowed

prettyShow :: PrettyShow a => a -> String
prettyShow = runPrettyShow . pshow

prettyShowIndent :: PrettyShow a
                 => Indentation         -- starting indentation level
                 -> String              -- indentation string
                 -> a
                 -> String
prettyShowIndent n str = runPrettyShowIndent n str . pshow

runPrettyShow :: PrettyShowed -> String
runPrettyShow = runPrettyShowIndent 0 "    "

runPrettyShowIndent :: Indentation      -- starting indentation level
                    -> String           -- indentation string
                    -> PrettyShowed
                    -> String
runPrettyShowIndent n str = views ps_lines showLines . flip execState initialST
    where
        showLines :: [(Indentation, String)] -> String
        showLines = intercalate "\n" . fmap indent . reverse
            where
                indent :: (Indentation, String) -> String
                indent (ind, str') = tabs ind ++ str'
                tabs :: Indentation -> String
                tabs ind = concat (replicate ind str)
        initialST :: PrettyShowState
        initialST = PrettyShowState
                { _ps_level = n
                , _ps_lines  = [(n, "")]
                }

----------------------------------------

pshows :: (PrettyShow a, Foldable f) => PrettyShowed -> f a -> PrettyShowed
pshows act xs = case toList xs of
        p : ps -> pshow p >> mapM_ (\x -> act >> pshow x) ps
        _      -> return ()

string :: String -> PrettyShowed
string str = ps_lines._head._2 %= (++ str)

lstring :: Lens' s String -> s -> PrettyShowed
lstring lns = views lns string

line :: PrettyShowed
line = use ps_level >>= \n -> ps_lines %= cons (n, "")

raise :: PrettyShowed
raise = ps_level += 1

lower :: PrettyShowed
lower = ps_level -= 1
