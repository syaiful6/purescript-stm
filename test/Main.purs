module Test.Main where

import Prelude

import Control.Monad.Aff (Aff, forkAff, launchAff, delay)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.STM (STMEff, AffSTM, TVar, STM, newTVar, writeTVar, readTVar, atomically)
import Control.Plus (empty)
import Control.Monad.Rec.Class (forever)

import Data.Array ((..))
import Data.Foldable (oneOf, sequence_)
import Data.Tuple (Tuple(..))
import Data.Newtype (wrap)
import Data.Int (toNumber)

type Effects eff = STMEff (console :: CONSOLE | eff)

assert :: forall eff. Boolean -> String -> Aff eff Unit
assert b s = unless b (throwError (error "Assertion failed"))

--------------------------------------------------------------------------------
-- Santa repeatedly sleeps until wakened by either all of his nine reindeer,
-- back from their holidays, or by a group of three of his ten elves. If awakened
-- by the reindeer, he harnesses each of them to his sleigh, delivers toys with
-- them and finally unharnesses them (allowing them to go off on holiday). If awakened
-- by a group of elves, he shows each of the group into his study, consults with them
-- on toy R&D and finally shows them each out (allowing them to go back to work).
-- Santa should give priority to the reindeer in the case that there is both a group of elves
-- and a group of reindeer waiting.
--------------------------------------------------------------------------------

data Gate = Gate Int (TVar Int)

newGate :: Int -> STM Gate
newGate n = do
  tv <- newTVar 0
  pure (Gate n tv)

passGate :: forall eff. Gate -> AffSTM eff Unit
passGate (Gate n tv) = atomically do
  left <- readTVar tv
  _    <- check (left > 0)
  writeTVar tv (left - 1)

operateGate :: forall eff. Gate -> AffSTM eff Unit
operateGate (Gate n tv) = do
  _ <- atomically (writeTVar tv n)
  atomically do
    left <- readTVar tv
    check (left == 0)

data Pair3 a b c = Pair3 a b c

data Group = Group Int (TVar (Pair3 Int Gate Gate))

newGroup :: forall eff. Int -> AffSTM eff Group
newGroup n = atomically do
  pair <- Pair3 n <$> newGate n <*> newGate n
  tv   <- newTVar pair
  pure $ Group n tv

joinGroup :: forall eff. Group -> AffSTM eff (Tuple Gate Gate)
joinGroup (Group n tv) = atomically do
  Pair3 left g1 g2 <- readTVar tv
  _ <- check (left > 0)
  _ <- writeTVar tv (Pair3 (left - 1) g1 g2)
  pure $ Tuple g1 g2

awaitGroup :: Group -> STM (Tuple Gate Gate)
awaitGroup (Group n tv) = do
  Pair3 left g1 g2 <- readTVar tv
  _ <- check (left == 0)
  ng1 <- newGate n
  ng2 <- newGate n
  _ <- writeTVar tv (Pair3 n ng1 ng2)
  pure $ Tuple g1 g2

check :: Boolean -> STM Unit
check true = pure unit
check _    = empty

choose :: forall a eff. Array (Tuple (STM a) (a -> AffSTM eff Unit)) -> AffSTM eff Unit
choose choices = do
  todo <- atomically (oneOf actions)
  todo
  where
  f :: (a -> AffSTM eff Unit) -> STM a -> STM (AffSTM eff Unit)
  f rhs guard = do
    v <- guard
    pure (rhs v)
  actions :: Array (STM (AffSTM eff Unit))
  actions = do
    Tuple guard rhs <- choices
    pure (f rhs guard)

randomDelay :: forall eff. Aff eff Unit
randomDelay = do
  wait <- liftEff $ toNumber <$> _randomMili 1 10000
  delay (wrap wait)

foreign import _randomMili :: forall eff. Int -> Int -> Eff eff Int

meetInStudy :: forall r. Int -> Aff (console :: CONSOLE | r) Unit
meetInStudy i = log ("Elf " <> show i<>  " meeting in the study\n")

deliverToys :: forall r. Int -> Aff (console :: CONSOLE | r) Unit
deliverToys i = log ("Reindeer " <>  show i <>  " delivering toys\n")

wrapTask :: forall eff. Group -> AffSTM eff Unit -> AffSTM eff Unit
wrapTask group task = do
  Tuple inGate outGate <- joinGroup group
  _ <- passGate inGate
  _ <- task
  passGate outGate

elfTask :: forall eff. Group -> Int -> Aff (Effects eff) Unit
elfTask group i = wrapTask group (meetInStudy i)

reindeerTask :: forall eff. Group -> Int -> Aff (Effects eff) Unit
reindeerTask group i = wrapTask group (deliverToys i)

santa :: forall eff. Group -> Group -> Aff (Effects eff) Unit
santa elf rein = do
  log "----------\n"
  choose
    [ Tuple (awaitGroup rein) (run "deliver toys"),
      Tuple (awaitGroup elf)  (run "meet in my study")
    ]
  where
  run :: String -> Tuple Gate Gate -> Aff (Effects eff) Unit
  run task (Tuple ingate outGate) = do
    log ("Ho! Ho! Ho! let's " <> task <> "\n")
    operateGate ingate
    operateGate outGate

main :: Eff (Effects (exception :: EXCEPTION)) Unit
main = void $ launchAff do
  log "The Santa Claus problem.\n"
  elfg <- newGroup 3
  sequence_ do
    n <- 1..10
    pure $ spawn elfTask elfg n
  reing <- newGroup 9
  sequence_ do
    n <- 1..9
    pure $ spawn reindeerTask reing n
  -- run forever
  forever (santa elfg reing)
  where
  spawn f gp i = forkAff $ forever do
    f gp i
    randomDelay
