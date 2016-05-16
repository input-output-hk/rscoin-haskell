{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}

-- | RSCoin.Test.MonadTimed specification

module Test.RSCoin.Timed.MonadTimedSpec
       ( spec
       ) where

import           Control.Exception.Base      (Exception)
import           Control.Concurrent.MVar     (newEmptyMVar, putMVar, takeMVar)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import           Control.Monad               (void)
import           Control.Monad.State         (StateT, execStateT, modify, put)
import           Control.Monad.Trans         (MonadIO, liftIO)
import           Control.Monad.Catch         (MonadCatch, throwM, handleAll,
                                              catchAll, catch)
import           Data.Typeable               (Typeable)
import           Numeric.Natural             (Natural)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, counterexample, ioProperty,
                                              (===), NonNegative (..))
import           Test.QuickCheck.Function    (Fun, apply)
import           Test.QuickCheck.Monadic     (PropertyM, assert, monadic, monitor,
                                              run)
import           Test.QuickCheck.Poly        (A)
import           Test.RSCoin.Timed.Arbitrary ()

import           RSCoin.Timed.MonadTimed     (Microsecond, MonadTimed (..), fork_,
                                              RelativeToNow, invoke, now, schedule,
                                              for, after, sec, MonadTimedError, mcs)
import           RSCoin.Timed.TimedIO        (TimedIO, runTimedIO)
import           RSCoin.Timed.Timed          (TimedT, runTimedT)

spec :: Spec
spec =
    describe "MonadTimed" $ do
        describe "now" $ do
            prop "should return the same time as specified"
                nowProp
        monadTimedSpec "TimedIO" runTimedIOProp
        monadTimedTSpec "TimedT" runTimedTProp

monadTimedSpec
    :: (MonadTimed m, MonadIO m, MonadCatch m)
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
-- TODO: fix tests for timeout in TimedIO.
--        describe "timeout" $ do
--            prop "should throw an exception if time has exceeded" $
--                \a -> runProp . timeoutProp a


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
        describe "timeout" $ do
            prop "should throw an exception if time has exceeded" $
                \a -> runProp . timeoutTimedProp a
        describe "killThread" $ do
            prop "should abort the execution of a thread" $
                \a b -> runProp . killThreadTimedProp a b
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

timeoutProp
    :: (MonadTimed m, MonadIO m, MonadCatch m)
    => NonNegative Microsecond
    -> NonNegative Microsecond
    -> PropertyM m ()
timeoutProp (getNonNegative -> tout) (getNonNegative -> wt) = do
    let action = do
            wait $ for wt mcs
            return $ wt <= tout
        handler (_ :: MonadTimedError) = return $ wt >= tout
    res <- run $ timeout tout action `catch` handler
    assert res

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
forkSemanticProp = actionSemanticProp fork_

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
-- TODO: As TimedT is an instance of MonadIO, we can now reuse tests for TimedIO instead of these tests

-- TODO: use checkpoints timeout pattern from ExceptionSpec
killThreadTimedProp
    :: NonNegative Microsecond
    -> NonNegative Microsecond
    -> NonNegative Microsecond
    -> TimedTProp ()
killThreadTimedProp (getNonNegative -> mTime) (getNonNegative -> f1Time) (getNonNegative -> f2Time)= do
    var <- liftIO $ newTVarIO (0 :: Int)
    tId <- fork $ do
        fork_ $ do -- this thread can't be killed
            wait $ for f1Time mcs
            liftIO $ atomically $ writeTVar var 1
        wait $ for f2Time mcs
        liftIO $ atomically $ writeTVar var 2
    wait $ for mTime mcs
    killThread tId
    wait $ for f1Time mcs f2Time mcs -- wait for both threads to finish
    res <- liftIO $ readTVarIO var
    assertTimedT $ check res
  where
    check 0 = mTime <= f1Time && mTime <= f2Time
    check 1 = True -- this thread can't be killed
    check 2 = f2Time <= mTime
    check _ = error "This checkpoint doesn't exist"

timeoutTimedProp
    :: NonNegative Microsecond
    -> NonNegative Microsecond
    -> TimedTProp ()
timeoutTimedProp (getNonNegative -> tout) (getNonNegative -> wt) = do
    liftIO $ print (tout, wt)
    liftIO $ print 0
    let action = do
            liftIO $ print 1
            wait $ for wt mcs
            liftIO $ print 2
            return $ wt <= tout
        handler (_ :: MonadTimedError) = do
            liftIO $ print 3
            return $ tout <= wt
    res <- timeout tout action `catch` handler
    liftIO $ print 4
    liftIO $ print res
    assertTimedT res

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
forkSemanticTimedProp = actionSemanticTimedProp fork_

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

nowProp :: Microsecond -> Property
nowProp ms = 0 === now ms


-- * Excpetions

data TestException = TestExc
    deriving (Show, Typeable)

instance Exception TestException

handleAll' :: MonadCatch m => m () -> m ()
handleAll' = handleAll $ const $ return ()


exceptionsThrown :: TimedTProp ()
exceptionsThrown = handleAll' $ do
    void $ throwM TestExc
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
    fork_ $ throwM TestExc
    wait $ for 1 sec
    put True

exceptionNotAffectOtherThread :: TimedTProp ()
exceptionNotAffectOtherThread = handleAll' $ do
    put False
    schedule (after 3 sec) $ put True
    schedule (after 1 sec) $ throwM TestExc
