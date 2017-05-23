module Control.Monad.STM
  ( module Control.Monad.STM.Internal
  ) where

import Prelude
import Control.Plus (empty)

import Control.Monad.STM.Internal (STMEff, TVar, STM, atomically, AffSTM, validateTVar)

-- | Retry a transaction if condition doesn't hold
check :: Boolean -> STM Unit
check b = if b then pure unit else empty

-- `orElse` combinator is not defined because you can use <|>
-- `retry` combinator is not defined, instead use empty method defined Control.Plus
