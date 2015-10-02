{-# LANGUAGE RankNTypes #-}

module Language.Angler.Monad
    ( STWarnings(..)
    , warn
    ) where

import           Language.Angler.Error
import           Language.Angler.SrcLoc

import           Control.Lens
import           Control.Monad.State    (MonadState)

class STWarnings s where
        st_warnings :: Lens' s [Located Warning]

warn :: (STWarnings s, MonadState s m) => Located Warning -> m ()
warn w = st_warnings %= (|> w)
