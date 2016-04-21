{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
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
import           Test.QuickCheck.Poly        (A)

import           RSCoin.Test.MonadTimed      (RelativeToNow, MicroSeconds, now,
                                              after, MonadTimed (..), runTimedIO,
                                              TimedIO, schedule, invoke)

spec :: Spec
spec =
    describe "MonadTimed" $do
        describe "now" $ do
            prop "should return the same time as specified" nowProp
        describe "localTime >> localTime" $ do
            prop "first localTime will run before second localTime" localTimePassingProp
        describe "wait t" $ do
            prop "will wait at least t" waitPassingProp
        describe "fork" $ do
            prop "won't change semantics of an action" forkSemanticProp
        describe "schedule" $ do
            prop "won't change semantics of an action, will execute action in the future" scheduleSemanticProp
        describe "invoke" $ do
            prop "won't change semantics of an action, will execute action in the future" invokeSemanticProp

instance Show (a -> a) where
    show = const "Dummy method show"

-- TODO: figure out how to test recursive functions like after/at

invokeSemanticProp
    :: RelativeToNow
    -> A
    -> (A -> A)
    -> Property
invokeSemanticProp = actionTimeSemanticProp invoke

scheduleSemanticProp
    :: RelativeToNow
    -> A
    -> (A -> A)
    -> Property
scheduleSemanticProp = actionTimeSemanticProp schedule

actionTimeSemanticProp
    :: (RelativeToNow -> TimedIO () -> TimedIO ())
    -> RelativeToNow
    -> A
    -> (A -> A)
    -> Property
actionTimeSemanticProp action relativeToNow init modify =
         actionSemanticProp action' init modify  
    .&&. timePassingProp relativeToNow (action' $ pure ())
  where
    action' = action relativeToNow

forkSemanticProp :: A -> (A -> A) -> Property
forkSemanticProp = actionSemanticProp fork

waitPassingProp :: RelativeToNow -> Property
waitPassingProp relativeToNow =
    timePassingProp relativeToNow $ wait relativeToNow

localTimePassingProp :: Property
localTimePassingProp =
    timePassingProp now $ pure ()

timePassingProp :: RelativeToNow -> TimedIO () -> Property
timePassingProp relativeToNow action = ioProperty . runTimedIO $ do
    t1 <- localTime
    action
    t2 <- localTime
    return $ relativeToNow t1 <= t2

actionSemanticProp
    :: (TimedIO () -> TimedIO ())
    -> A
    -> (A -> A)
    -> Property
actionSemanticProp action init modify = ioProperty . runTimedIO $ do
    tvar <- liftIO $ newTVarIO init
    action $ liftIO $ atomically $ modifyTVar tvar modify
    liftIO $ (modify init ==) <$> readTVarIO tvar

nowProp :: MicroSeconds -> Bool
nowProp ms = ms == now ms
