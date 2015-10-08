{-# LANGUAGE RankNTypes #-}

module Language.Angler.Monad
    ( foldActions

    , pushM, peekM, popM

    , STWarnings(..)
    , warn
    ) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc

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
