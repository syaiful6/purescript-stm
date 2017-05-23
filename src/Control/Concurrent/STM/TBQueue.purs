module Control.Concurrent.STM.TBQueue
  ( TBQueue
  , newTBQueue
  , newTBQueueAff
  , readTBQueue
  , tryReadTBQueue
  , peekTBQueue
  , tryPeekTBQueue
  , writeTBQueue
  , unGetTBQueue
  , isEmptyTBQueue
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.STM (STM, AffSTM)
import Control.Concurrent.STM.TVar (TVar, newTVar, newTVarAff, readTVar, writeTVar)

import Data.List (List(Nil), (:), reverse)
import Data.Maybe (Maybe(..))

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
-- | It uses two lists to obtain amortised /O(1)/, enqueue and dequeue operations.
data TBQueue a = TBQueue (TVar Int) (TVar (List a)) (TVar Int) (TVar (List a))

instance eqTBQueue :: Eq (TBQueue a) where
  eq (TBQueue a _ _ _) (TBQueue b _ _ _) = a == b

-- | Build and returns a new instance of 'TBQueue' by providing it maximum capacity
newTBQueue :: forall a. Int -> STM (TBQueue a)
newTBQueue size = TBQueue
  <$> newTVar 0    -- CR: read capacity
  <*> newTVar Nil  -- R:  elements waiting to be read
  <*> newTVar size -- CW: write capacity
  <*> newTVar Nil  -- W:  elements written (head is most recent)

newTBQueueAff :: forall eff a. Int -> AffSTM eff (TBQueue a)
newTBQueueAff size = TBQueue
  <$> newTVarAff 0
  <*> newTVarAff Nil
  <*> newTVarAff size
  <*> newTVarAff Nil

-- | Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: forall a. TBQueue a -> a -> STM Unit
writeTBQueue (TBQueue rsize _read wsize write) a = do
  w <- readTVar wsize
  if (w /= 0)
   then do writeTVar wsize (w - 1)
   else do
    r <- readTVar rsize
    if (r /= 0)
       then do writeTVar rsize 0
               writeTVar wsize (r - 1)
       else empty
  listend <- readTVar write
  writeTVar write (a:listend)

-- | Read the next value from the 'TBQueue'.
readTBQueue :: forall a. TBQueue a -> STM a
readTBQueue (TBQueue rsize read _wsize write) = do
  xs <- readTVar read
  r  <- readTVar rsize
  _  <- writeTVar rsize (r + 1)
  case xs of
    (x : xs')  -> writeTVar read xs' $> x
    Nil        -> do
      ys <- readTVar write
      case ys of
        Nil -> empty -- retry
        _   -> do
          let rys = reverse ys
          _ <- writeTVar write Nil
          case rys of
            Nil       -> empty
            (z : zs)  -> writeTVar read zs $> z

-- | A version of 'readTBQueue' which does not retry. Instead it
-- | returns `Nothing` if no value is available.
tryReadTBQueue :: forall a. TBQueue a -> STM (Maybe a)
tryReadTBQueue c = map Just (readTBQueue c) <|> pure Nothing

-- | Get the next value from the TBQueue without removing it,
-- | retrying if the channel is empty.
peekTBQueue :: forall a. TBQueue a -> STM a
peekTBQueue c = do
  x <- readTBQueue c
  _ <- unGetTBQueue c x
  pure x

-- | A version of 'peekTBQueue' which does not retry. Instead it
-- | returns Nothing if no value is available.
tryPeekTBQueue :: forall a. TBQueue a -> STM (Maybe a)
tryPeekTBQueue c = do
  m <- tryReadTBQueue c
  case m of
    Nothing -> pure Nothing
    Just x  -> do
      _ <- unGetTBQueue c x
      pure m

-- | Put a data item back onto a channel, where it will be the next item read.
-- Blocks if the queue is full.
unGetTBQueue :: forall a. TBQueue a -> a -> STM Unit
unGetTBQueue (TBQueue rsize read wsize _write) a = do
  r <- readTVar rsize
  if (r > 0)
    then do writeTVar rsize (r - 1)
    else do
      w <- readTVar wsize
      if (w > 0)
        then writeTVar wsize (w - 1)
        else empty
  xs <- readTVar read
  writeTVar read (a:xs)

-- | Returns 'True' if the supplied 'TBQueue' is empty.
isEmptyTBQueue :: forall a. TBQueue a -> STM Boolean
isEmptyTBQueue (TBQueue _rsize read _wsize write) = do
  xs <- readTVar read
  case xs of
    (_ : _)  -> pure false
    Nil      -> do
      ys <- readTVar write
      case ys of
        Nil -> pure true
        _   -> pure false

-- | Returns 'True' if the supplied 'TBQueue' is full.
isFullTBQueue :: forall a. TBQueue a -> STM Boolean
isFullTBQueue (TBQueue rsize _read wsize _write) = do
  w <- readTVar wsize
  if (w > 0)
   then pure false
   else do
      r <- readTVar rsize
      if (r > 0)
        then pure false
        else pure true