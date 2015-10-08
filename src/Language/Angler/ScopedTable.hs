module Language.Angler.ScopedTable
        ( ScopedTable
        , tab_stack

        -- basic
        , empty
        , lookup, (!) , elem
        , safeInsert, insert

        -- scope handling
        , enterScope, exitScope

        -- on keys
        , mapKeys
        , filterByKey
        ) where

import           Language.Angler.Error

import           Control.Lens               hiding (op)

import           Data.Foldable              (msum)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map

import           Prelude                    hiding (elem, lookup)
import qualified Prelude                    as P (elem)

-- interface for a scoped symbol table
newtype ScopedTable sym
  = ScopedTable
        { _tab_stack    :: [ Map String sym ] }

makeLenses ''ScopedTable

empty :: ScopedTable sym
empty = ScopedTable [ Map.empty ]

enterScope :: ScopedTable sym -> ScopedTable sym
enterScope = over tab_stack (cons Map.empty)

exitScope :: ScopedTable sym -> ScopedTable sym
exitScope = over tab_stack tail

lookup :: String -> ScopedTable sym -> Maybe sym
lookup str = msum . fmap (Map.lookup str) . view tab_stack

(!) :: ScopedTable sym -> String -> sym
(!) tab = maybe message id . flip lookup tab
    where
        message = error "SymbolTable.!: given key is not an element in the symbol table"

elem :: String -> ScopedTable sym -> Bool
elem str = any (P.elem str . Map.keys) . view tab_stack

safeInsert :: String -> sym -> ScopedTable sym -> Either Error (ScopedTable sym)
safeInsert str sym tab = if elem str tab
        then (Left . CheckError . AlreadyInSymbolTable) str
        else Right (over (tab_stack._head) (Map.insert str sym) tab)

insert :: String -> sym -> ScopedTable sym -> ScopedTable sym
insert str sym tab = if elem str tab
        then (error . show . CheckError . AlreadyInSymbolTable) str
        else over (tab_stack._head) (Map.insert str sym) tab

mapKeys :: (String -> String) -> ScopedTable sym -> ScopedTable sym
mapKeys f = over (tab_stack.traverse) (Map.mapKeys f)

filterByKey :: (String -> Bool) -> ScopedTable sym -> ScopedTable sym
filterByKey f = over (tab_stack.traverse) (Map.filterWithKey (\s _ -> f s))
