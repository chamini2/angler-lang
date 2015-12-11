{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Language.Angler.Monad
    ( foldActions

    , pushM, peekM, popM

    , STWarnings(..)
    , warn

    , STErrors(..)
    , addError, addLErr, addPErr, addCErr

    , throwError

    , STScopedTable(..)
    , lookupSc, lookupAndHandleSc
    , insertSc, insertAndHandleSc, replaceSc
    , enterScWith, enterSc, exitSc, bracketScWith, bracketSc, topSc
    ) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable

import           Control.Lens

import           Control.Monad               (when)
import           Control.Monad.State         (MonadState)
import           Control.Monad.Except        (throwError)

import           Data.Foldable               (foldl')
import           Data.Maybe                  (isJust, isNothing, fromMaybe)

import           Prelude                     hiding (lookup)

foldActions :: Monad m => [a -> m a] -> a -> m a
foldActions acts x = foldl' (>>=) (return x) acts

--------------------------------------------------------------------------------

pushM :: MonadState s m => Lens' s [a] -> a -> m ()
pushM lns x = lns %= cons x

peekM :: MonadState s m => Lens' s [a] -> m a
peekM lns = fromMaybe (error "Monad.peekM: Nothing") <$> preuse (lns._head)

popM  :: MonadState s m => Lens' s [a] -> m ()
popM lns = lns %= tail

--------------------------------------------------------------------------------

class STWarnings st where
        st_warnings :: Lens' st [Located Warning]

warn :: (STWarnings s, MonadState s m) => Located Warning -> m ()
warn w = st_warnings %= (|> w)

----------------------------------------

class STErrors st where
        st_errors :: Lens' st [Located Error]

addError :: (STErrors s, MonadState s m) => Error -> SrcSpan -> m ()
addError e s = st_errors %= (|> Loc s e)

addLErr :: (STErrors s, MonadState s m) => LexError -> SrcSpan -> m ()
addLErr = addError . LexError

addPErr :: (STErrors s, MonadState s m) => ParseError -> SrcSpan -> m ()
addPErr = addError . ParseError

addCErr :: (STErrors s, MonadState s m) => CheckError -> SrcSpan -> m ()
addCErr = addError . CheckError

--------------------------------------------------------------------------------

class STScopedTable st sym | st -> sym where
        st_table :: Lens' st (ScopedTable sym)

lookupSc :: (STScopedTable s sym, MonadState s m) => String -> m (Maybe sym)
lookupSc = uses st_table . lookup

lookupAndHandleSc :: (STScopedTable s sym, MonadState s m, STErrors s)
                  => String -> SrcSpan -> m (Maybe sym)
lookupAndHandleSc str spn = do
        msym <- lookupSc str
        when (isNothing msym) $ addCErr (CErrNotInSymbolTable str) spn
        return msym

insertSc :: (STScopedTable s sym, MonadState s m) => String -> sym -> m (Maybe Error)
insertSc str sym = do
        eitTab <- uses st_table (safeInsert str sym)
        case eitTab of
                Left err   -> return (Just err)
                Right tab' -> assign st_table tab' >> return Nothing

insertAndHandleSc :: (STScopedTable s sym, MonadState s m, STErrors s)
                  => String -> sym -> SrcSpan -> m ()
insertAndHandleSc str sym spn = do
        merr <- insertSc str sym
        when (isJust merr) $ do
                let Just err = merr
                addError err spn

replaceSc :: (STScopedTable s sym, MonadState s m) => String -> sym -> m ()
replaceSc str sym = st_table %= replace str sym

enterScWith :: (STScopedTable s sym, MonadState s m) => Scope sym -> m ()
enterScWith up = st_table %= enterScopeWith up

enterSc :: (STScopedTable s sym, MonadState s m) => m ()
enterSc = st_table %= enterScope

exitSc :: (STScopedTable s sym, MonadState s m) => m ()
exitSc = st_table %= exitScope

bracketScWith :: (STScopedTable s sym, MonadState s m) => Scope sym -> m a -> m a
bracketScWith up act = enterScWith up >> act >>= \r -> exitSc >> return r

bracketSc :: (STScopedTable s sym, MonadState s m) => m a -> m a
bracketSc = bracketScWith emptyScope

topSc :: (STScopedTable s sym, MonadState s m) => m (Scope sym)
topSc = uses st_table topScope
