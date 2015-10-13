{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Language.Angler.Monad
    ( foldActions

    , pushM, peekM, popM

    , STWarnings(..)
    , warn

    , throwError

    , STScopedTable(..)
    , lookupSc, safeLookupSc
    , insertSc, safeInsertSc
    , enterSc, exitSc, actInNewSc
    ) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc
import           Language.Angler.ScopedTable

import           Control.Lens
import           Control.Monad.State         (MonadState)
import           Control.Monad.Except        (throwError)
import           Prelude                     hiding (lookup)

foldActions :: Monad m => [a -> m a] -> a -> m a
foldActions acts x = foldl (>>=) (return x) acts

--------------------------------------------------------------------------------

pushM :: MonadState s m => Lens' s [a] -> a -> m ()
pushM lns x = lns %= cons x

peekM :: MonadState s m => Lens' s [a] -> m a
peekM lns = maybe (error "Monad.peekM: Nothing") id <$> preuse (lns._head)

popM  :: MonadState s m => Lens' s [a] -> m ()
popM lns = lns %= tail

--------------------------------------------------------------------------------

class STWarnings st where
        st_warnings :: Lens' st [Located Warning]

warn :: (STWarnings s, MonadState s m) => Located Warning -> m ()
warn w = st_warnings %= (|> w)

--------------------------------------------------------------------------------

class STScopedTable st sym | st -> sym where
        st_table :: Lens' st (ScopedTable sym)

lookupSc :: (STScopedTable s sym, MonadState s m) => String -> m sym
lookupSc = uses st_table . flip (!)

safeLookupSc :: (STScopedTable s sym, MonadState s m) => String -> m (Maybe sym)
safeLookupSc = uses st_table . lookup

insertSc :: (STScopedTable s sym, MonadState s m) => String -> sym -> m ()
insertSc str sym = st_table %= insert str sym

safeInsertSc :: (STScopedTable s sym, MonadState s m) => String -> sym -> m (Maybe Error)
safeInsertSc str sym = do
        tab <- use st_table
        case safeInsert str sym tab of
                Left err   -> return (Just err)
                Right tab' -> assign st_table tab' >> return Nothing

enterSc :: (STScopedTable s sym, MonadState s m) => m ()
enterSc = st_table %= enterScope

exitSc :: (STScopedTable s sym, MonadState s m) => m ()
exitSc = st_table %= exitScope

actInNewSc :: (STScopedTable s sym, MonadState s m) => m a -> m a
actInNewSc act = enterSc >> act >>= \r -> exitSc >> return r
