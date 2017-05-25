module Control.Monad.STM
  ( module Control.Monad.STM.Internal

  , module Control.Monad.STM.TBQueue
  , module Control.Monad.STM.TQueue
  , module Control.Monad.STM.TVar
  , module Control.Monad.STM.TMVar
  ) where

import Control.Monad.STM.Internal (STMEff, TVar, STM, AffSTM, atomically, validateTVar, check, retry)

import Control.Monad.STM.TVar (modifyTVar, swapTVar, newTVar, newTVarAff, readTVar, readTVarEff
  , writeTVar, writeTVarAff)
import Control.Monad.STM.TQueue (TQueue, newTQueue, newTQueueAff, readTQueue, tryReadTQueue
  , peekTQueue, tryPeekTQueue, writeTQueue, unGetTQueue, isEmptyTQueue)
import Control.Monad.STM.TBQueue (TBQueue, newTBQueue, newTBQueueAff, readTBQueue, tryReadTBQueue
  , peekTBQueue, tryPeekTBQueue, writeTBQueue, unGetTBQueue, isEmptyTBQueue)
import Control.Monad.STM.TMVar (TMVar, newTMVar, newEmptyTMVar, newTMVarAff, newEmptyTMVarAff
  , takeTMVar, putTMVar, readTMVar, tryReadTMVar, swapTMVar, tryTakeTMVar, tryPutTMVar, isEmptyTMVar)

-- `orElse` combinator is not defined because you can use <|> instead
