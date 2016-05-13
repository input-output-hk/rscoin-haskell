{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.RSCoin.Timed.ExceptionSpec
       ( spec
       ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar, newTVarIO,
                                              readTVarIO, writeTVar)
import           Control.Exception.Base      (ArithException (Overflow),
                                              AsyncException (ThreadKilled))
import           Control.Monad               (void)
import           Control.Monad.Catch         (MonadCatch, catch, catchAll,
                                              throwM)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Random.Class  (getRandomR)
import           Control.Monad.Trans         (MonadIO)
import           Data.Default                (def)
import           Data.Time.Units             (fromMicroseconds)
import           System.Random               (StdGen, mkStdGen)
import           Test.Hspec                  (Spec, before, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (..), NonNegative (..),
                                              Property)
import           Test.QuickCheck.Monadic     (assert, monadicIO)
import           Test.QuickCheck.Property    (Result (reason), exception,
                                              failed, ioProperty, succeeded)

import           RSCoin.Core                 (Severity (Error), initLogging)
import           RSCoin.Timed                (Delays (..), Microsecond, PureRpc,
                                              after, for, fork, invoke, mcs,
                                              runEmulationMode_, sec, wait)

import           RSCoin.User.Error           (UserError (InputProcessingError))
import           Test.RSCoin.Timed.Arbitrary ()

spec :: Spec
spec =
    before (initLogging Error) $
    describe "WorkMode" $ do
        describe "error" $ do
            prop "should abort the execution"
                exceptionShouldAbortExecution
            prop "caught nicely"
                excCaught
            prop "exception from main thread caught nicely outside of monad"
                excCaughtOutside
            prop "proper catch order (catch inside catch)"
                excCatchOrder
            prop "(wait + throw) - exception doesn't get lost"
                excWaitThrow
            prop "(wait + throw) in forked thread - exception doesn't get lost"
                excWaitThrowForked
            prop "catch doesn't get handle future exceptions"
                excCatchScope
            prop "different exceptions, catch inner"
                excDiffCatchInner
            prop "different exceptions, catch outer"
                excDiffCatchOuter
        describe "async error" $
            prop "shouldn't abort the execution"
                asyncExceptionShouldntAbortExecution

exceptionShouldAbortExecution
    :: StdGen
    -> NonNegative Microsecond
    -> Property
exceptionShouldAbortExecution std (getNonNegative -> t) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        runEmulationMode_ (Just std) delays' $
            fork $ do
                liftIO $ atomically $ writeTVar var 1
                wait $ for t mcs
                void $ throwM $ InputProcessingError "Error"
                liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 1

asyncExceptionShouldntAbortExecution
    :: StdGen
    -> NonNegative Microsecond
    -> NonNegative Microsecond
    -> Property
asyncExceptionShouldntAbortExecution std (getNonNegative -> t1) (getNonNegative -> t2) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        runEmulationMode_ (Just std) delays' $ do
            liftIO $ atomically $ writeTVar var 1
            fork $ do
                wait $ for t2 mcs
                throwM $ InputProcessingError "Error"
            wait $ for t1 mcs
            liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 2

excCaught
    :: StdGen
    -> Property
excCaught seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act   = throwM ThreadKilled >> checkPoint (-1)
                hnd _ = checkPoint 1
            in  act `catchAll` hnd

excCaughtOutside
    :: StdGen
    -> Property
excCaughtOutside seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint ->
            let act = runEmu seed (throwM ThreadKilled) >> checkPoint (-1)
                hnd _ = checkPoint 1
            in  act `catchAll` hnd

excWaitThrow
    :: StdGen
    -> Property
excWaitThrow seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act = do
                    wait (for 1 sec)
                    throwM ThreadKilled
                hnd _ = checkPoint 1
            in  do
                    act `catchAll` hnd
                    checkPoint 2

excWaitThrowForked
    :: StdGen
    -> Property
excWaitThrowForked seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act = do
                    wait (for 1 sec)
                    throwM ThreadKilled
                hnd _ = checkPoint 1
            in  do
                    fork $ act `catchAll` hnd
                    invoke (after 1 sec) $ checkPoint 2

excCatchOrder
    :: StdGen
    -> Property
excCatchOrder seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act    = throwM ThreadKilled
                hnd1 _ = checkPoint 1
                hnd2 _ = checkPoint (-1)
            in  do  act `catchAll` hnd1 `catchAll` hnd2
                    checkPoint 2

excCatchScope
    :: StdGen
    -> Property
excCatchScope seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act1 = checkPoint 1 `catchAll` const (checkPoint $ -1)
                act2 = act1 >> throwM ThreadKilled
            in  do
                act2 `catchAll` const (checkPoint 2)
                checkPoint 3

excDiffCatchInner
    :: StdGen
    -> Property
excDiffCatchInner seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act = throwM ThreadKilled
                hnd1 (_ :: AsyncException) = checkPoint 1
                hnd2 (_ :: ArithException) = checkPoint (-1)
            in  do
                act `catch` hnd1 `catch` hnd2
                checkPoint 2

excDiffCatchOuter
    :: StdGen
    -> Property
excDiffCatchOuter seed =
    ioProperty . inSandbox . withCheckPoints $
        \checkPoint -> runEmu seed $
            let act = throwM Overflow
                hnd1 (_ :: AsyncException) = checkPoint (-1)
                hnd2 (_ :: ArithException) = checkPoint 1
            in  do
                act `catch` hnd1 `catch` hnd2
                checkPoint 2


-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

-- FIXME: use arbitrary instead of StdGen
delays' :: Delays
delays' = Delays d
  where
    d _ _ = Just . fromMicroseconds <$> getRandomR (10, 1000)


runEmu :: StdGen -> PureRpc IO () -> IO ()
runEmu seed = runEmulationMode_ (Just seed) def

-- Principle of checkpoints: every checkpoint has it's id
-- Checkpoints should be visited in according order: 1, 2, 3 ...
newtype CheckPoints = CP { getCP :: TVar (Either String Int) }

initCheckPoints :: MonadIO m => m CheckPoints
initCheckPoints = fmap CP $ liftIO $ newTVarIO $ Right 0

visitCheckPoint :: MonadIO m => CheckPoints -> Int -> m ()
visitCheckPoint cp curId = liftIO $ atomically $ modifyTVar (getCP cp) $
    \wasId ->
        if wasId == Right (curId - 1)
            then Right curId
            else Left $ either id (showError curId) wasId
  where
    showError cur was = mconcat
        ["Wrong chechpoint. Expected "
        , show (was + 1)
        , ", but visited "
        , show cur
        ]

assertCheckPoints :: MonadIO m => CheckPoints -> m Result
assertCheckPoints = fmap mkRes . liftIO . readTVarIO . getCP
  where
    mkRes (Left msg) = failed { reason = msg }
    mkRes (Right _)  = succeeded

withCheckPoints :: MonadIO m => ((Int -> m ()) -> IO a) -> IO Result
withCheckPoints act = do
    cp <- initCheckPoints
    _  <- act $ liftIO . visitCheckPoint cp
    assertCheckPoints cp

inSandbox :: MonadCatch m => m Result -> m Result
inSandbox = flip catchAll $ return . exception "Unexpected exception"
