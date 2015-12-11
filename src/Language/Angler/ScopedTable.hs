{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Angler.ScopedTable
        ( ScopedTable, Scope
        , tab_stack

        -- basic
        , emptyScope, emptyWithIndefinable, empty
        , lookup , elem, elemInCurrentScope
        , insertWith, safeInsert
        , adjust, replace

        -- scope handling
        , enterScopeWith, enterScope
        , topScope, exitScope

        , toList
        , fromFoldable, safeFromFoldable

        -- on keys
        , mapKeys
        , filterByKey
        ) where

import           Language.Angler.Error

import           PrettyShow

import           Control.Lens               hiding (op)

import           Data.Foldable              (msum)
import qualified Data.Map.Strict            as Map

import           Prelude                    hiding (elem, lookup)
import qualified Prelude                    as P (elem)

type Scope sym = Map.Map String sym

-- interface for a scoped symbol table
data ScopedTable sym
  = ScopedTable
        { _tab_stack    :: [ Scope sym ]
        , _indefinable  :: [ String ]
        }
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''ScopedTable

emptyScope :: Scope sym
emptyScope = Map.empty

emptyWithIndefinable :: [String] -> ScopedTable sym
emptyWithIndefinable = ScopedTable [emptyScope]

empty :: ScopedTable sym
empty = emptyWithIndefinable []

enterScopeWith :: Scope sym -> ScopedTable sym -> ScopedTable sym
enterScopeWith up = over tab_stack (cons up)

enterScope :: ScopedTable sym -> ScopedTable sym
enterScope = enterScopeWith emptyScope

topScope :: ScopedTable sym -> Scope sym
topScope = views tab_stack head

exitScope :: ScopedTable sym -> ScopedTable sym
exitScope = over tab_stack tail

lookup :: String -> ScopedTable sym -> Maybe sym
lookup str = views tab_stack (msum . fmap (Map.lookup str))

scopeElem :: String -> Scope sym -> Bool
scopeElem str = P.elem str . Map.keys

elem :: String -> ScopedTable sym -> Bool
elem str = views tab_stack (any (scopeElem str))

elemInCurrentScope :: String -> ScopedTable sym -> Bool
elemInCurrentScope str = views tab_stack (scopeElem str . head)

insertWith :: (sym -> sym -> sym) -> String -> sym -> ScopedTable sym -> ScopedTable sym
insertWith join str sym = over (tab_stack._head) (Map.insertWith join str sym)

safeInsert :: String -> sym -> ScopedTable sym -> Either Error (ScopedTable sym)
safeInsert str sym tab = if P.elem str (view indefinable tab) || elemInCurrentScope str tab
        then (Left . CheckError . CErrAlreadyInSymbolTable) str
        else Right (insert str sym tab)

-- overwrites the symbol in the top scope
insert :: String -> sym -> ScopedTable sym -> ScopedTable sym
insert = insertWith const

-- looks for the symbol and adjusts it in the appropiate scope
adjust :: forall sym . (sym -> sym) -> String -> ScopedTable sym  -> ScopedTable sym
adjust f str = over tab_stack adjust'
    where
        adjust' :: [Scope sym] -> [Scope sym]
        adjust' scopes = case scopes of
                sc : scs -> if scopeElem str sc
                        then Map.adjust f str sc : scs
                        else sc : adjust' scs
                []     -> []

replace :: String -> sym -> ScopedTable sym -> ScopedTable sym
replace str sym = adjust (const sym) str

toList :: ScopedTable sym -> [(String, sym)]
toList = views tab_stack (proccess . concatMap Map.toList)
    where
        proccess :: [(String, sym)] -> [(String, sym)]
        proccess = Map.toList . foldr (uncurry Map.insert) emptyScope

fromFoldable :: Foldable f => f (String, sym) -> ScopedTable sym
fromFoldable = foldl (flip (uncurry insert)) empty

safeFromFoldable :: Foldable f => f (String, sym) -> Either Error (ScopedTable sym)
safeFromFoldable = foldl (\act (str,sym) -> act >>= safeInsert str sym) (Right empty)

mapKeys :: (String -> String) -> ScopedTable sym -> ScopedTable sym
mapKeys f = over (tab_stack.traverse) (Map.mapKeys f)

filterByKey :: (String -> Bool) -> ScopedTable sym -> ScopedTable sym
filterByKey f = over (tab_stack.traverse) (Map.filterWithKey (\s _ -> f s))

instance PrettyShow sym => PrettyShow (ScopedTable sym) where
        pshow = pshows line . (map snd . toList)
