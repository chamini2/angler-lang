module Language.Angler.SymbolTable
        ( SymbolTable, SymbolTableSpan

        , Symbol(..), SymbolSpan

        , Map.empty
        , Map.lookup, (Map.!)
        , insert
        , Map.delete

        , elem
        , Map.mapKeys
        , Map.filterWithKey
        ) where

import           Language.Angler.AST
import           Language.Angler.SrcLoc

-- import           Control.Lens

import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map

import           Prelude                hiding (elem, notElem)
import qualified Prelude                as P (elem)

type SymbolTable a = Map String (Symbol a)
type SymbolTableSpan = SymbolTable SrcSpan

data Symbol a
  = Symbol
        { _sym_type     :: Expression a
        , _sym_val      :: Expression a
        , _sym_annot    :: a
        }
type SymbolSpan = Symbol SrcSpan

insert :: String -> Symbol a -> SymbolTable a -> (SymbolTable a, Bool)
insert idn sym tab = (Map.insert idn sym tab, notElem idn tab)

elem :: String -> SymbolTable a -> Bool
elem idn = P.elem idn . Map.keys

notElem :: String -> SymbolTable a -> Bool
notElem idn = not . elem idn
