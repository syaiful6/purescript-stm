module Control.Concurrent.STM.TQueue
  ( TQueue
  , newTQueue
  , newTQueueAff
  , readTQueue
  , tryReadTQueue
  , peekTQueue
  , tryPeekTQueue
  , writeTQueue
  , unGetTQueue
  , isEmptyTQueue
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.STM (STM, AffSTM)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarAff, readTVar, writeTVar)

import Data.List (List(Nil), (:), reverse)
import Data.Maybe (Maybe(..))

-- | 'TQueue' is an abstract type representing an unbounded FIFO channel.
-- | It uses two lists to obtain amortised /O(1)/, enqueue and dequeue operations.
data TQueue a = TQueue (TVar (List a)) (TVar (List a))

instance eqTQueue :: Eq (TQueue a) where
  eq (TQueue a _) (TQueue b _) = a == b

-- | Build and returns a new instance of 'TQueue'
newTQueue :: forall a. STM (TQueue a)
newTQueue = TQueue
  <$> newTVar Nil
  <*> newTVar Nil

newTQueueAff :: forall eff a. AffSTM eff (TQueue a)
newTQueueAff = TQueue
  <$> newTVarAff Nil
  <*> newTVarAff Nil

-- | Write a value to a 'TQueue'.
writeTQueue :: forall a. TQueue a -> a -> STM Unit
writeTQueue (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

-- | Read the next value from the 'TQueue'.
readTQueue :: forall a. TQueue a -> STM a
readTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x : xs')  -> writeTVar read xs' $> x
    Nil        -> do
      ys <- readTVar write
      case ys of
        Nil -> empty -- retry
        _   -> case reverse ys of
          Nil      -> empty
          (z : zs) -> writeTVar write Nil *> writeTVar read zs $> z

-- | A version of 'readTQueue' which does not retry. Instead it
-- | returns Nothing if no value is available.
tryReadTQueue :: forall a. TQueue a -> STM (Maybe a)
tryReadTQueue c = map Just (readTQueue c) <|> pure Nothing

-- | Get the next value from the TQueue without removing it,
-- | retrying if the channel is empty.
peekTQueue :: forall a. TQueue a -> STM a
peekTQueue c = do
  x <- readTQueue c
  _ <- unGetTQueue c x
  pure x

-- | A version of 'peekTQueue' which does not retry. Instead it
-- | returns Nothing if no value is available.
tryPeekTQueue :: forall a. TQueue a -> STM (Maybe a)
tryPeekTQueue c = do
  m <- tryReadTQueue c
  case m of
    Nothing -> pure Nothing
    Just x  -> do
      _ <- unGetTQueue c x
      pure m

-- | Put a data item back onto a channel, where it will be the next item read.
unGetTQueue :: forall a. TQueue a -> a -> STM Unit
unGetTQueue (TQueue read _write) a = do
  xs <- readTVar read
  writeTVar read (a:xs)

-- | Returns 'True' if the supplied 'TQueue' is empty.
isEmptyTQueue :: forall a. TQueue a -> STM Boolean
isEmptyTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (_ : _) -> pure false
    Nil      -> do
      ys <- readTVar write
      case ys of
        Nil -> pure true
        _   -> pure false