module SymbolTable
        ( SymbolTable(..)
        , Symbol(..)
        , lookup
        ) where

import           Language.Angler.AST

import           Control.Lens
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (isJust)
import           Prelude             hiding (lookup)

type SymbolTable a = Map String (Symbol a)

data Symbol a
  = Symbol
        { _sym_type     :: Expression
        , _sym_val      :: Expression
        , _sym_annot    :: a
        }

lookup :: String -> SymbolTable a -> Maybe (Symbol a)
lookup idn = Map.lookup idn

(!) :: SymbolTable a -> String -> Symbol a
(!) = check . flip lookup
    where
        check ms = case ms of
                Just x  -> x
                Nothing -> error ("SymbolTable.!: identifier " ++ idn ++
                                  " is not an element in the table")

elem :: String -> SymbolTable a -> Bool
elem = isJust . lookup

insert :: String -> Symbol a -> SymbolTable a -> (SymbolTable a, Bool)
insert idn sym tab = (Map.insert idn sym tab, not (elem idn tab))
