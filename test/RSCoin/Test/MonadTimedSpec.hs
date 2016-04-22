{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | RSCoin.Test.MonadTimed specification

module RSCoin.Test.MonadTimedSpec
       ( spec
       ) where

import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, modifyTVar)
import           Control.Concurrent.STM      (atomically)
import           Control.Monad.Trans         (liftIO, lift, MonadIO)
import           Numeric.Natural             (Natural)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), 
                                             Property, ioProperty,
                                             counterexample)
import           Test.QuickCheck.Function    (Fun, apply) 
import           Test.QuickCheck.Monadic     (run, assert, PropertyM, monadic,
                                             monitor)
import           Test.QuickCheck.Poly        (A)

import           RSCoin.Test.MonadTimed      (MicroSeconds, now, RelativeToNow,
                                              after, MonadTimed (..), runTimedIO,
                                              TimedIO, schedule, invoke)
import           RSCoin.Test.Timed           (runTimedT, TimedT)

spec :: Spec
spec =
    describe "MonadTimed" $ do
        describe "TimedIO" $ do
            describe "now" $ do
                prop "should return the same time as specified" 
                    nowProp
            describe "localTime >> localTime" $ do
                prop "first localTime will run before second localTime" $
                    runTimedIOProp localTimePassingProp
            describe "wait t" $ do
                prop "will wait at least t" $
                    runTimedIOProp . waitPassingProp
            describe "fork" $ do
                prop "won't change semantics of an action" $
                    \a -> runTimedIOProp . forkSemanticProp a
            describe "schedule" $ do
                prop "won't change semantics of an action, will execute action in the future" $
                    \a b -> runTimedIOProp . scheduleSemanticProp a b
            describe "invoke" $ do
                prop "won't change semantics of an action, will execute action in the future" $
                    \a b -> runTimedIOProp . invokeSemanticProp a b

type RelativeToNowNat = Natural

fromIntegralRTN :: RelativeToNowNat -> RelativeToNow
fromIntegralRTN = (+) . fromIntegral

-- TODO: figure out how to test recursive functions like after/at

runTimedIOProp :: PropertyM TimedIO () -> Property
runTimedIOProp = monadic $ ioProperty . runTimedIO

-- runTimedTProp :: PropertyM (TimedT IO) () -> Property
-- runTimedTProp = monadic $ ioProperty . runTimedT

invokeSemanticProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNowNat
    -> A
    -> Fun A A
    -> PropertyM m ()
invokeSemanticProp = actionTimeSemanticProp invoke

scheduleSemanticProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNowNat
    -> A
    -> Fun A A
    -> PropertyM m ()
scheduleSemanticProp = actionTimeSemanticProp schedule

actionTimeSemanticProp
    :: (MonadTimed m, MonadIO m)
    => (RelativeToNow -> m () -> m ())
    -> RelativeToNowNat
    -> A
    -> Fun A A
    -> PropertyM m ()
actionTimeSemanticProp action relativeToNow init f = do
    actionSemanticProp action' init f  
    timePassingProp relativeToNow . action' $ pure ()
  where
    action' = action $ fromIntegralRTN relativeToNow

forkSemanticProp
    :: (MonadTimed m, MonadIO m)
    => A
    -> Fun A A
    -> PropertyM m ()
forkSemanticProp = actionSemanticProp fork

waitPassingProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNowNat
    -> PropertyM m ()
waitPassingProp relativeToNow =
    timePassingProp relativeToNow . wait $ fromIntegralRTN relativeToNow

localTimePassingProp :: (MonadTimed m, MonadIO m) => PropertyM m ()
localTimePassingProp =
    timePassingProp 0 $ pure ()

-- | Proves that at least relativeToNow has passed while executing an action
timePassingProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNowNat
    -> m ()
    -> PropertyM m ()
timePassingProp relativeToNow action = do
    t1 <- run localTime
    run action
    t2 <- run localTime
    monitor (counterexample $ mconcat 
        [ "t1: ", show t1
        , ", t2: ", show t2, ", "
        , show $ fromIntegralRTN relativeToNow t1, " <= ", show t2
        ])
    assert $ fromIntegralRTN relativeToNow t1 <= t2

-- | Proves that an action will be executed
actionSemanticProp
    :: (MonadTimed m, MonadIO m)
    => (m () -> m ())
    -> A
    -> Fun A A
    -> PropertyM m ()
actionSemanticProp action init f = do
    tvar <- liftIO $ newTVarIO init
    run . action . liftIO . atomically . modifyTVar tvar $ apply f
    modvar <- liftIO $ readTVarIO tvar
    assert $ apply f init == modvar

nowProp :: MicroSeconds -> Bool
nowProp ms = ms == now ms
