{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | RSCoin.Test.MonadTimed specification

module Test.RSCoin.Timed.MonadTimedSpec
       ( spec
       ) where

import           Control.Exception.Base   (Exception, SomeException)
import           Control.Concurrent.MVar  (newEmptyMVar, putMVar, takeMVar)
import           Control.Monad.State      (StateT, execStateT, modify, put)
import           Control.Monad.Trans      (MonadIO, liftIO)
import           Control.Monad.Catch      (MonadCatch, throwM, catch, handleAll,
                                           catchAll)
import           Control.Monad.Catch.Pure (CatchT, runCatchT)
import           Data.Typeable            (Typeable)
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
                                           RelativeToNow, invoke, now, schedule,
                                           for, after, sec)
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
    -> (TimedTProp () -> Property)
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
        describe "exceptions" $ do
            prop "thrown nicely" $
                runProp exceptionsThrown
            prop "caught nicely" $
                runProp exceptionsThrowCaught
            prop "wait + throw caught nicely" $
                runProp exceptionsWaitThrowCaught
            prop "exceptions don't affect main thread" $
                runProp exceptionNotAffectMainThread 
            prop "exceptions don't affect other threads" $
                runProp exceptionNotAffectOtherThread 


-- pure version
-- type TimedTProp = TimedT (CatchT (State Bool))
type TimedTProp = TimedT (StateT Bool IO)

assertTimedT :: Bool -> TimedTProp ()
assertTimedT b = modify (b &&)

type RelativeToNowNat = Natural

fromIntegralRTN :: RelativeToNowNat -> RelativeToNow
fromIntegralRTN = (+) . fromIntegral

-- TODO: figure out how to test recursive functions like after/at

runTimedIOProp :: PropertyM TimedIO () -> Property
runTimedIOProp = monadic $ ioProperty . runTimedIO

runTimedTProp :: TimedTProp () -> Property
runTimedTProp test = ioProperty $ execStateT (runTimedT test) True

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


-- * Excpetions

data TestException = TestExc 
    deriving (Show, Typeable)

instance Exception TestException

handleAll' :: MonadCatch m => m () -> m ()
handleAll' = handleAll $ const $ return ()


exceptionsThrown :: TimedTProp ()
exceptionsThrown = handleAll' $ do
    throwM TestExc 
    put False

exceptionsThrowCaught :: TimedTProp ()
exceptionsThrowCaught = 
    let act = do
            put False 
            throwM TestExc
        hnd = const $ put True
    in  act `catchAll` hnd

exceptionsWaitThrowCaught :: TimedTProp ()
exceptionsWaitThrowCaught = 
    let act = do
            put False 
            wait $ for 1 sec
            throwM TestExc
        hnd = const $ put True
    in  act `catchAll` hnd 

exceptionNotAffectMainThread :: TimedTProp ()
exceptionNotAffectMainThread = handleAll' $ do
    put False
    fork $ throwM TestExc
    wait $ for 1 sec
    put True

exceptionNotAffectOtherThread :: TimedTProp ()
exceptionNotAffectOtherThread = handleAll' $ do
    put False
    schedule (after 3 sec) $ put True
    schedule (after 1 sec) $ throwM TestExc

