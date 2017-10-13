module Control.Monad.STM.Internal
  ( STMEff
  , AffSTM
  , TVar
  , STM
  , atomically
  , newTVar
  , newTVarAff
  , readTVar
  , readTVarEff
  , writeTVar
  , writeTVarAff
  , validateTVar
  , check
  , retry
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Aff (Aff, delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError, catchError)
import Control.Monad.Aff.AVar as AV
import Control.MonadPlus (class MonadZero, class MonadPlus)
import Control.Monad.State.Trans (StateT, runStateT)
import Control.Monad.State.Class as ST
import Control.Monad.Trans.Class (lift)
import Control.Plus (class Plus, empty)

import Data.List (List(Nil), (:), null, catMaybes)
import Data.Foldable (for_, or, and, minimum)
import Data.Traversable (traverse, for)
import Data.Maybe (Maybe(..), maybe, fromJust)
import Data.Map as M
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

-- | effect rows
type STMEff r = (avar :: AV.AVAR, ref :: Ref.REF | r)

-- | type synonym for Aff used by STM
type AffSTM r = Aff (STMEff r)

-- | unique value of TVar
type TVarId = Int

type Timestamp = Number

type Set a = M.Map a Unit

data TVar a = TVar TVarId (AV.AVar Unit) (Ref.Ref Timestamp) (Ref.Ref a) (Ref.Ref (List (AV.AVar Unit)))

newtype ATVar = ATVar (forall a. TVar a)

data Result a
  = Retry
  | Abort
  | Good a

type TRec =
  { curTime  :: Timestamp
  , topTime  :: (Maybe Timestamp)
  , writeSet :: (Set ATVar)
  , curSet   :: (M.Map ATVar Timestamp)
  , cache    :: (M.Map ATVar Any)
  }

-- | The STM Monad.
newtype STM a = STM (forall r. StateT TRec (Aff (STMEff r)) (Result a))

-- | create a new ```TVar``` by give it initial value.
newTVar :: forall a. a -> STM (TVar a)
newTVar val = STM do
  tvar <- lift (newTVarAff val)
  modifyCache (M.insert (aTVar tvar) (mkAny val))
  pure (Good tvar)

newTVarAff :: forall eff a. a -> AffSTM eff (TVar a)
newTVarAff a = TVar
  <$> liftEff _newTVarId
  <*> AV.makeVar unit
  <*> liftEff (Ref.newRef 0.00)
  <*> liftEff (Ref.newRef a)
  <*> liftEff (Ref.newRef Nil)

readTVar :: forall a. TVar a -> STM a
readTVar tvar@(TVar _ lock timeRef valRef _) = STM do
  cache' <- _.cache <$> ST.get
  case M.lookup (aTVar tvar) cache' of
    Just val -> pure $ Good $ unsafeCoerce val
    Nothing  -> do
      {time, val} <- lift $ withMVar lock $ \_ ->
          {time: _, val: _}
          <$> liftEff (Ref.readRef timeRef)
          <*> liftEff (Ref.readRef valRef)
      _ <- modifyCurSet (M.insert (aTVar tvar) time)
      _ <- modifyCache (M.insert (aTVar tvar) (mkAny val))
      curTime' <- _.curTime <$> ST.get
      when (time > curTime') $ modifyCurTime (const time)
      topTimeM <- _.topTime <$> ST.get
      let
        cmpT x' = curTime' + 1.0 > x'
        success = time <= curTime' || maybe true cmpT topTimeM
      pure $ if success then Good val else Abort

-- | The read operation return Eff instead Aff as we don't need to lock
readTVarEff :: forall e a. TVar a -> Eff (ref :: Ref.REF | e) a
readTVarEff (TVar _ _ _ ref _) = Ref.readRef ref

writeTVar :: forall a. TVar a -> a -> STM Unit
writeTVar tvar val = do
  _ <- readTVar tvar
  STM do
    _ <- modifyCache (M.insert (aTVar tvar) (mkAny val))
    _ <- modifyWriteSet (M.insert (aTVar tvar) unit)
    pure (Good unit)

writeTVarAff :: forall e a. TVar a -> a -> AffSTM e Unit
writeTVarAff (TVar _ lock timeRef valRef _) val = withMVar lock \_ -> void $
  liftEff do
    Ref.modifyRef timeRef ((+) 1.00)
    Ref.writeRef valRef val

validateTVar :: forall a. TVar a -> STM Unit
validateTVar tvar@(TVar _ _ timeRef _ _) = STM do
  newTime <- liftEff $ Ref.readRef timeRef
  curSet' <- _.curSet <$> ST.get
  case M.lookup (aTVar tvar) curSet' of
    Nothing -> pure (Good unit)
    Just oldTime
      | oldTime == newTime -> pure (Good unit)
      | otherwise          -> do
          _ <- modifyCurSet (M.delete (aTVar tvar))
          curTime' <- _.curTime <$> ST.get
          if curTime' + 1.00 < newTime
            then do
              _ <- modifyTopTime (Just <<< maybe newTime (min newTime))
              pure (Good unit)
            else pure Abort

-- | Retry a transaction if condition doesn't hold
check :: Boolean -> STM Unit
check b = if b then pure unit else empty

-- | Retry combinator just an alias to `empty`. Retry execution of the current
-- | memory transaction because it has seen values in TVars which mean that it
-- | should not continue (e.g. the TVars represent a shared buffer that is now empty).
retry :: forall a. STM a
retry = empty

atomically :: forall e a. STM a -> AffSTM e a
atomically (STM act) = go
  where
  shouldAbort :: AV.AVar Unit -> TRec -> ATVar -> AffSTM e Boolean
  shouldAbort waitLock rec atvar@(ATVar (TVar _ lock timeRef _ waitLocksRef)) =
    withMVar lock \_ -> do
      time <- liftEff $ Ref.readRef timeRef
      if Just time == M.lookup atvar rec.curSet
        then liftEff $ Ref.modifyRef waitLocksRef (waitLock : _) $> false
        else pure true

  setTolList :: forall b. Set b -> List b
  setTolList = M.keys

  mapToList :: forall k v. M.Map k v -> List (Tuple k v)
  mapToList = M.toUnfoldable

  commitTV :: TRec -> ATVar -> AffSTM e Unit
  commitTV rec' atvar = atvar # runATVar \(TVar _ _ timeRef valRef waitLocksRef) -> do
    let val = unsafePartial $ fromJust $ M.lookup atvar (rec'.cache)
    waitLocks <- liftEff do
      _ <- Ref.writeRef timeRef (rec'.curTime + 1.00)
      _ <- Ref.writeRef valRef (unsafeCoerce val)
      Ref.readRef waitLocksRef
    for_ waitLocks $ \waitLock -> AV.putVar unit waitLock
    liftEff $ Ref.writeRef waitLocksRef Nil

  go :: AffSTM e a
  go = do
    let
      trec :: TRec
      trec = { curTime: 0.00, topTime: Nothing, writeSet: M.empty, curSet: M.empty, cache: M.empty }
    Tuple maybeRet rec' <- runStateT act trec
    case maybeRet of
      Abort -> do
        -- | delay 15.00 milliseconds to allow js complete other task
        _ <- delay (wrap 15.00)
        go

      Retry -> do
        waitLock <- AV.makeEmptyVar :: (AffSTM e (AV.AVar Unit))
        immediateAbort <- or <$> traverse (shouldAbort waitLock rec') (M.keys rec'.cache)
        if immediateAbort
          then delay (wrap 15.00)  *> go
          else AV.takeVar waitLock *> go

      Good ret -> do
        _ <- for_ (M.keys rec'.cache) $ \(ATVar (TVar _ lock _ _ _)) -> AV.takeVar lock

        times <- for (mapToList rec'.curSet) $ \(Tuple (ATVar (TVar _ _ timeRef _ _)) recTime) -> do
          time <- liftEff $ Ref.readRef timeRef
          pure $ if time /= recTime then Just time else Nothing
        let
          prunedTimes = catMaybes (rec'.topTime : times)
          readSuccess = if null prunedTimes then true else Just (rec'.curTime + 1.00) < minimum prunedTimes
        writeSuccess <- and <$>
          (for (setTolList rec'.writeSet) $
            \atvar@(ATVar (TVar _ _ timeRef _ _)) -> do
              time <- liftEff $ Ref.readRef timeRef
              pure $ Just time == M.lookup atvar rec'.curSet)
        let success = readSuccess && writeSuccess
        if success
          then do
            for_ (setTolList (rec'.writeSet)) (commitTV rec')
            for_ (M.keys rec'.cache) $ \(ATVar (TVar _ lock _ _ _)) -> AV.putVar unit lock
            pure ret
          else do
            for_ (M.keys rec'.cache) $ \(ATVar (TVar _ lock _ _ _)) -> AV.putVar unit lock
            delay (wrap 15.00) *> go

instance eqTVar :: Eq (TVar a) where
  eq (TVar a _ _ _ _) (TVar b _ _ _ _) = a == b

-- | hide variable a. safe because ATVar is newtype
aTVar :: forall a. TVar a -> ATVar
aTVar = unsafeCoerce

runATVar :: forall r. (forall a. TVar a -> r) -> ATVar -> r
runATVar = unsafeCoerce

instance eqATVar :: Eq ATVar where
  eq (ATVar (TVar a _ _ _ _)) (ATVar (TVar b _ _ _ _)) = a == b

instance ordATVar :: Ord ATVar where
  compare (ATVar (TVar a _ _ _ _)) (ATVar (TVar b _ _ _ _)) = a `compare` b

foreign import data Any :: Type

mkAny :: forall a. a -> Any
mkAny = unsafeCoerce

instance functorSTM :: Functor STM where
  map f (STM st) = STM (f' <$> st)
    where
    f' (Good a) = Good (f a)
    f' Retry    = Retry
    f' Abort    = Abort

instance applySTM :: Apply STM where
  apply = ap

instance applicativeSTM :: Applicative STM where
  pure a = STM (pure (Good a))

instance bindSTM :: Bind STM where
  bind (STM mx) f = STM do
    x <- mx
    case x of
      Good b -> case f b of STM b' -> b'
      Retry  -> pure Retry
      Abort  -> pure Abort

instance monadSTM :: Monad STM

instance altSTM :: Alt STM where
  alt (STM left) (STM right) = STM do
    lv <- left
    case lv of
      Retry -> right
      _     -> pure lv

instance plusSTM :: Plus STM where
  empty = STM (pure Retry)

instance alternativeSTM :: Alternative STM

instance monadZeroSTM :: MonadZero STM

instance monadPlusSTM :: MonadPlus STM

modifyCurTime :: forall e. (Timestamp -> Timestamp) -> StateT TRec (AffSTM e) Unit
modifyCurTime f = ST.modify $ \rec -> rec { curTime = f rec.curTime }

modifyTopTime :: forall e. (Maybe Timestamp -> Maybe Timestamp) -> StateT TRec (AffSTM e) Unit
modifyTopTime f = ST.modify $ \rec-> rec { topTime = f rec.topTime }

modifyWriteSet :: forall e. (Set ATVar -> Set ATVar) -> StateT TRec (AffSTM e) Unit
modifyWriteSet f = ST.modify $ \rec -> rec { writeSet = f rec.writeSet }

modifyCurSet :: forall e. (M.Map ATVar Timestamp -> M.Map ATVar Timestamp) -> StateT TRec (AffSTM e) Unit
modifyCurSet f = ST.modify $ \rec -> rec { curSet = f rec.curSet }

modifyCache :: forall e. (M.Map ATVar Any -> M.Map ATVar Any) -> StateT TRec (AffSTM e) Unit
modifyCache f = ST.modify $ \rec -> rec { cache = f rec.cache }

withMVar :: forall e a b. AV.AVar a -> (a -> Aff (avar :: AV.AVAR | e) b) -> Aff (avar :: AV.AVAR | e) b
withMVar v aff = do
  a <- AV.takeVar v
  b <- aff a `catchError` \e -> AV.putVar a v *> throwError e
  AV.putVar a v *> pure b

foreign import _newTVarId :: forall eff. Eff eff Int
