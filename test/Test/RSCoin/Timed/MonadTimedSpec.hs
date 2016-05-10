{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | RSCoin.Test.MonadTimed specification

module Test.RSCoin.Timed.MonadTimedSpec
       ( spec
       ) where

import           Control.Concurrent.MVar  (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.State      (State, execState, modify)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Control.Monad.Catch.Pure (CatchT, runCatchT)
import           Numeric.Natural          (Natural)
import           Test.Hspec               (Spec, describe)
import           Test.Hspec.QuickCheck    (prop)
import           Test.QuickCheck          (Property, counterexample, ioProperty,
                                           (===))
import           Test.QuickCheck.Function (Fun, apply)
import           Test.QuickCheck.Monadic  (PropertyM, assert, monadic, monitor,
                                           run)
import           Test.QuickCheck.Poly     (A)

import           RSCoin.Timed.MonadTimed  (MicroSeconds, MonadTimed (..),
                                           RelativeToNow, invoke, now, schedule)
import           RSCoin.Timed.TimedIO     (TimedIO, runTimedIO)
import           RSCoin.Timed.Timed       (TimedT, runTimedT)

spec :: Spec
spec =
    describe "MonadTimed" $ do
        describe "now" $ do
            prop "should return the same time as specified"
                nowProp
        monadTimedSpec "TimedIO" runTimedIOProp
        monadTimedTSpec "TimedT" runTimedTProp

monadTimedSpec
    :: (MonadTimed m, MonadIO m)
    => String
    -> (PropertyM m () -> Property)
    -> Spec
monadTimedSpec description runProp =
    describe description $ do
        describe "localTime >> localTime" $ do
            prop "first localTime will run before second localTime" $
                runProp localTimePassingProp
        describe "wait t" $ do
            prop "will wait at least t" $
                runProp . waitPassingProp
        describe "fork" $ do
            prop "won't change semantics of an action" $
                \a -> runProp . forkSemanticProp a
        describe "schedule" $ do
            prop "won't change semantics of an action, will execute action in the future" $
                \a b -> runProp . scheduleSemanticProp a b
        describe "invoke" $ do
            prop "won't change semantics of an action, will execute action in the future" $
                \a b -> runProp . invokeSemanticProp a b

monadTimedTSpec
    :: String
    -> (TimedTProp () -> Bool)
    -> Spec
monadTimedTSpec description runProp =
    describe description $ do
        describe "localTime >> localTime" $ do
            prop "first localTime will run before second localTime" $
                runProp localTimePassingTimedProp
        describe "wait t" $ do
            prop "will wait at least t" $
                runProp . waitPassingTimedProp
        describe "fork" $ do
            prop "won't change semantics of an action" $
                \a -> runProp . forkSemanticTimedProp a
        describe "schedule" $ do
            prop "won't change semantics of an action, will execute action in the future" $
                \a b -> runProp . scheduleSemanticTimedProp a b
        describe "invoke" $ do
            prop "won't change semantics of an action, will execute action in the future" $
                \a b -> runProp . invokeSemanticTimedProp a b


type TimedTProp = TimedT (CatchT (State Bool))

assertTimedT :: Bool -> TimedTProp ()
assertTimedT b = modify (b &&)

type RelativeToNowNat = Natural

fromIntegralRTN :: RelativeToNowNat -> RelativeToNow
fromIntegralRTN = (+) . fromIntegral

-- TODO: figure out how to test recursive functions like after/at

runTimedIOProp :: PropertyM TimedIO () -> Property
runTimedIOProp = monadic $ ioProperty . runTimedIO

runTimedTProp :: TimedTProp () -> Bool
runTimedTProp test = execState (runCatchT $ runTimedT test) True

-- TimedIO tests

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
actionTimeSemanticProp action relativeToNow val f = do
    actionSemanticProp action' val f
    timePassingProp relativeToNow action'
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
    timePassingProp relativeToNow (wait (fromIntegralRTN relativeToNow) >>)

localTimePassingProp :: (MonadTimed m, MonadIO m) => PropertyM m ()
localTimePassingProp =
    timePassingProp 0 id

-- TODO: instead of testing with MVar's we should create PropertyM an instance of MonadTimed.
-- With that we could test inside forked/waited actions
-- | Tests that action will be exececuted after relativeToNow
timePassingProp
    :: (MonadTimed m, MonadIO m)
    => RelativeToNowNat
    -> (m () -> m ())
    -> PropertyM m ()
timePassingProp relativeToNow action = do
    mvar <- liftIO newEmptyMVar
    t1 <- run localTime
    run . action $ localTime >>= liftIO . putMVar mvar
    t2 <- liftIO $ takeMVar mvar
    monitor (counterexample $ mconcat
        [ "t1: ", show t1
        , ", t2: ", show t2, ", "
        , show $ fromIntegralRTN relativeToNow t1, " <= ", show t2
        ])
    assert $ fromIntegralRTN relativeToNow t1 <= t2

-- | Tests that an action will be executed
actionSemanticProp
    :: (MonadTimed m, MonadIO m)
    => (m () -> m ())
    -> A
    -> Fun A A
    -> PropertyM m ()
actionSemanticProp action val f = do
    mvar <- liftIO newEmptyMVar
    run . action . liftIO . putMVar mvar $ apply f val
    result <- liftIO $ takeMVar mvar
    monitor (counterexample $ mconcat
        [ "f: ", show f
        , ", val: ", show val
        , ", f val: ", show $ apply f val
        , ", should be: ", show result
        ])
    assert $ apply f val == result

-- TimedT tests

invokeSemanticTimedProp
    :: RelativeToNowNat
    -> A
    -> Fun A A
    -> TimedTProp ()
invokeSemanticTimedProp = actionTimeSemanticTimedProp invoke

scheduleSemanticTimedProp
    :: RelativeToNowNat
    -> A
    -> Fun A A
    -> TimedTProp ()
scheduleSemanticTimedProp = actionTimeSemanticTimedProp schedule

actionTimeSemanticTimedProp
    :: (RelativeToNow -> TimedTProp () -> TimedTProp ())
    -> RelativeToNowNat
    -> A
    -> Fun A A
    -> TimedTProp ()
actionTimeSemanticTimedProp action relativeToNow val f = do
    actionSemanticTimedProp action' val f
    timePassingTimedProp relativeToNow action'
  where
    action' = action $ fromIntegralRTN relativeToNow

forkSemanticTimedProp
    :: A
    -> Fun A A
    -> TimedTProp ()
forkSemanticTimedProp = actionSemanticTimedProp fork

waitPassingTimedProp
    :: RelativeToNowNat
    -> TimedTProp ()
waitPassingTimedProp relativeToNow =
    timePassingTimedProp relativeToNow (wait (fromIntegralRTN relativeToNow) >>)

localTimePassingTimedProp :: TimedTProp ()
localTimePassingTimedProp =
    timePassingTimedProp 0 id

-- | Tests that action will be exececuted after relativeToNow
timePassingTimedProp
    :: RelativeToNowNat
    -> (TimedTProp () -> TimedTProp ())
    -> TimedTProp ()
timePassingTimedProp relativeToNow action = do
    t1 <- localTime
    action $ localTime >>= assertTimedT . (fromIntegralRTN relativeToNow t1 <=)

-- | Tests that an action will be executed
actionSemanticTimedProp
    :: (TimedTProp () -> TimedTProp ())
    -> A
    -> Fun A A
    -> TimedTProp ()
actionSemanticTimedProp action val f = do
    let result = apply f val
    action $ assertTimedT $ apply f val == result

nowProp :: MicroSeconds -> Property
nowProp ms = 0 === now ms
