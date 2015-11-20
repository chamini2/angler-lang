module Language.Angler.ScopedTable
        ( ScopedTable
        , tab_stack

        -- basic
        , empty
        , lookup , elem, elemInCurrentScope
        , insertWith, safeInsert

        -- scope handling
        , enterScope, exitScope

        , toList
        , fromFoldable, safeFromList

        -- on keys
        , mapKeys
        , filterByKey
        ) where

import           Language.Angler.Error

import           Control.Lens               hiding (op)

import           Data.Foldable              (msum)
import qualified Data.Map.Strict            as Map

import           Prelude                    hiding (elem, lookup)
import qualified Prelude                    as P (elem)

-- interface for a scoped symbol table
newtype ScopedTable sym
  = ScopedTable
        { _tab_stack    :: [Map.Map String sym] }
  deriving Show

makeLenses ''ScopedTable

empty :: ScopedTable sym
empty = ScopedTable [Map.empty]

enterScope :: ScopedTable sym -> ScopedTable sym
enterScope = over tab_stack (cons Map.empty)

exitScope :: ScopedTable sym -> ScopedTable sym
exitScope = over tab_stack tail

lookup :: String -> ScopedTable sym -> Maybe sym
lookup str = views tab_stack (msum . fmap (Map.lookup str))

elem :: String -> ScopedTable sym -> Bool
elem str = views tab_stack (any (P.elem str . Map.keys))

elemInCurrentScope :: String -> ScopedTable sym -> Bool
elemInCurrentScope str = views tab_stack (P.elem str . Map.keys . head)

insertWith :: (sym -> sym -> sym) -> String -> sym -> ScopedTable sym -> ScopedTable sym
insertWith join str sym = over (tab_stack._head) (Map.insertWith join str sym)

safeInsert :: String -> sym -> ScopedTable sym -> Either Error (ScopedTable sym)
safeInsert str sym tab = if elemInCurrentScope str tab
        then (Left . CheckError . CErrAlreadyInSymbolTable) str
        else Right (insert str sym tab)

-- This overwrites the symbol in the top scope
insert :: String -> sym -> ScopedTable sym -> ScopedTable sym
insert str sym = over (tab_stack._head) (Map.insert str sym)

toList :: ScopedTable sym -> [(String, sym)]
toList = views tab_stack (proccess . concatMap Map.toList)
    where
        proccess :: [(String, sym)] -> [(String, sym)]
        proccess = Map.toList . foldr (uncurry Map.insert) Map.empty

fromFoldable :: Foldable f => f (String, sym) -> ScopedTable sym
fromFoldable = foldl (flip (uncurry insert)) empty

safeFromList :: [(String, sym)] -> Either Error (ScopedTable sym)
safeFromList = foldl (\act (str,sym) -> act >>= safeInsert str sym) (Right empty)

mapKeys :: (String -> String) -> ScopedTable sym -> ScopedTable sym
mapKeys f = over (tab_stack.traverse) (Map.mapKeys f)

filterByKey :: (String -> Bool) -> ScopedTable sym -> ScopedTable sym
filterByKey f = over (tab_stack.traverse) (Map.filterWithKey (\s _ -> f s))
