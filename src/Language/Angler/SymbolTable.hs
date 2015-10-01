module Language.Angler.SymbolTable
        ( SymbolTable
        , Symbol(..)
        , lookup , (!)
        , elem, notElem
        ) where

import           Language.Angler.AST

-- import           Control.Lens
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (isJust)
import           Prelude             hiding (lookup, elem, notElem)

type SymbolTable a = Map String (Symbol a)

data Symbol a
  = Symbol
        { _sym_type     :: Expression a
        , _sym_val      :: Expression a
        , _sym_annot    :: a
        }

lookup :: String -> SymbolTable a -> Maybe (Symbol a)
lookup idn = Map.lookup idn

(!) :: SymbolTable a -> String -> Symbol a
(!) tab idn = check (lookup idn tab)
    where
        check ms = case ms of
                Just x  -> x
                Nothing -> error ("SymbolTable.!: identifier " ++ idn ++
                                  " is not an element in the table")

elem :: String -> SymbolTable a -> Bool
elem idn = isJust . lookup idn

notElem :: String -> SymbolTable a -> Bool
notElem idn = not . elem idn

insert :: String -> Symbol a -> SymbolTable a -> (SymbolTable a, Bool)
insert idn sym tab = (Map.insert idn sym tab, notElem idn tab)
