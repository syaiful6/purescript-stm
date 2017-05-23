module Control.Concurrent.STM.TVar
	( modifyTVar
  , swapTVar
  , module Control.Monad.STM.Internal
	) where

import Prelude

import Control.Monad.STM.Internal (STM, TVar, newTVar, newTVarAff, readTVar, readTVarEff
  , writeTVar, writeTVarAff)

-- |  Mutate the contents of a 'TVar'
modifyTVar :: forall a. TVar a -> (a -> a) -> STM Unit
modifyTVar var f = do
  x <- readTVar var
  writeTVar var (f x)

-- | Swap the contents of a 'TVar' for a new value.
swapTVar :: forall a. TVar a -> a -> STM a
swapTVar var new = do
  old <- readTVar var
  _   <- writeTVar var new
  pure old