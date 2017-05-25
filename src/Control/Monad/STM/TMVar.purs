module Control.Monad.STM.TMVar
  ( TMVar
  , newTMVar
  , newEmptyTMVar
  , newTMVarAff
  , newEmptyTMVarAff
  , takeTMVar
  , putTMVar
  , readTMVar
  , tryReadTMVar
  , swapTMVar
  , tryTakeTMVar
  , tryPutTMVar
  , isEmptyTMVar
  ) where

import Prelude

import Control.Monad.STM.Internal (STM, AffSTM, retry)
import Control.Monad.STM.TVar (TVar, newTVar, newTVarAff, readTVar, writeTVar)

import Data.Maybe (Maybe(..), maybe)

-- | TMVar: Transactional Aff AVars, for use in the STM monad.
newtype TMVar a = TMVar (TVar (Maybe a))

derive newtype instance eqTMVar :: Eq (TMVar a)

-- | Create a 'TMVar' which contains the supplied value.
newTMVar :: forall a. a -> STM (TMVar a)
newTMVar a = TMVar <$> newTVar (Just a)

newEmptyTMVar:: forall a. STM (TMVar a)
newEmptyTMVar = TMVar <$> newTVar Nothing

newTMVarAff :: forall eff a. a -> AffSTM eff (TMVar a)
newTMVarAff a = TMVar <$> newTVarAff (Just a)

newEmptyTMVarAff :: forall eff a. AffSTM eff (TMVar a)
newEmptyTMVarAff = TMVar <$> newTVarAff Nothing

takeTMVar :: forall a. TMVar a -> STM a
takeTMVar (TMVar t) = readTVar t >>= case _ of
  Nothing -> retry
  Just a  -> writeTVar t Nothing $> a

tryTakeTMVar :: forall a. TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar t) = readTVar t >>= case _ of
  Nothing -> pure Nothing
  Just a  -> writeTVar t Nothing $> Just a

putTMVar :: forall a. TMVar a -> a -> STM Unit
putTMVar (TMVar t) a = readTVar t >>= case _ of
  Nothing -> writeTVar t (Just a) $> unit
  Just _  -> retry

tryPutTMVar :: forall a. TMVar a -> a -> STM Boolean
tryPutTMVar (TMVar t) a = readTVar t >>= case _ of
  Nothing -> writeTVar t (Just a) $> true
  Just _  -> pure false

readTMVar :: forall a. TMVar a -> STM a
readTMVar (TMVar t) = readTVar t >>= case _ of
  Nothing -> retry
  Just a  -> pure a

tryReadTMVar :: forall a. TMVar a -> STM (Maybe a)
tryReadTMVar (TMVar t) = readTVar t

swapTMVar :: forall a. TMVar a -> a -> STM a
swapTMVar (TMVar t) new = readTVar t >>= case _ of
  Nothing  -> retry
  Just old -> writeTVar t (Just new) $> old

isEmptyTMVar :: forall a. TMVar a -> STM Boolean
isEmptyTMVar (TMVar t) = readTVar t >>= (pure <<< maybe true (const false))
