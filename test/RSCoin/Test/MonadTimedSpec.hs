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
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen, oneof,
                                             Property, ioProperty, (.&&.))
import           Test.QuickCheck.Monadic     (run, assert, PropertyM, monadic)
import           Test.QuickCheck.Poly        (A)

import           RSCoin.Test.MonadTimed      (RelativeToNow, MicroSeconds, now,
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

instance Show (a -> a) where
    show = const "Dummy method show"

-- TODO: figure out how to test recursive functions like after/at

runTimedIOProp :: PropertyM TimedIO () -> Property
runTimedIOProp = monadic $ ioProperty . runTimedIO

-- runTimedTProp :: PropertyM (TimedT IO) () -> Property
-- runTimedTProp = monadic $ ioProperty . runTimedT

invokeSemanticProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNow
    -> A
    -> (A -> A)
    -> PropertyM m ()
invokeSemanticProp = actionTimeSemanticProp invoke

scheduleSemanticProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNow
    -> A
    -> (A -> A)
    -> PropertyM m ()
scheduleSemanticProp = actionTimeSemanticProp schedule

actionTimeSemanticProp
    :: (MonadTimed m, MonadIO m)
    => (RelativeToNow -> m () -> m ())
    -> RelativeToNow
    -> A
    -> (A -> A)
    -> PropertyM m ()
actionTimeSemanticProp action relativeToNow init modify = do
    actionSemanticProp action' init modify  
    timePassingProp relativeToNow (action' $ pure ())
  where
    action' = action relativeToNow

forkSemanticProp
    :: (MonadTimed m, MonadIO m)
    => A
    -> (A -> A)
    -> PropertyM m ()
forkSemanticProp = actionSemanticProp $ fork

waitPassingProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNow
    -> PropertyM m ()
waitPassingProp relativeToNow =
    timePassingProp relativeToNow $ wait relativeToNow

localTimePassingProp :: (MonadTimed m, MonadIO m) => PropertyM m ()
localTimePassingProp =
    timePassingProp now $ pure ()

-- | Proves that at least relativeToNow has passed while executing an action
timePassingProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNow
    -> m ()
    -> PropertyM m ()
timePassingProp relativeToNow action = do
    t1 <- run localTime
    run action
    t2 <- run localTime
    assert $ relativeToNow t1 <= t2

-- | Proves that an action will be executed
actionSemanticProp
    :: (MonadTimed m, MonadIO m)
    => (m () -> m ())
    -> A
    -> (A -> A)
    -> PropertyM m ()
actionSemanticProp action init modify = do
    tvar <- liftIO $ newTVarIO init
    run . action . liftIO . atomically $ modifyTVar tvar modify
    modvar <- liftIO $ readTVarIO tvar
    assert $ modify init == modvar

nowProp :: MicroSeconds -> Bool
nowProp ms = ms == now ms
