{-# LANGUAGE ViewPatterns #-}

module RSCoin.Timed.ExceptionSpec
       ( spec
       ) where

import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad               (void)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import           Control.Monad.Catch         (throwM)
import           Control.Concurrent.STM      (atomically)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, Arbitrary (..),
                                              NonNegative (..))
import           Test.QuickCheck.Monadic     (assert, monadicIO)
import           System.Random               (StdGen, mkStdGen)
import           Control.Monad.Random.Class  (getRandomR)

import           RSCoin.Timed                (MicroSeconds, sec, for,
                                              wait, runEmulationMode,
                                              Delays (..), fork)

import           RSCoin.User.Error           (UserError (InputProcessingError))

spec :: Spec
spec =
    describe "WorkMode" $ do
        describe "error" $
            prop "should abort the execution"
                exceptionShouldAbortExecution
        describe "async error" $
            prop "shouldn't abort the execution"
                asyncExceptionShouldntAbortExecution

exceptionShouldAbortExecution
    :: StdGen
    -> NonNegative MicroSeconds
    -> Property
exceptionShouldAbortExecution std (getNonNegative -> t) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        liftIO $ runEmulationMode std delays' $ do
            liftIO $ atomically $ writeTVar var 1
            wait $ for t sec
            void $ throwM $ InputProcessingError "Error"
            liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 1

asyncExceptionShouldntAbortExecution
    :: StdGen
    -> NonNegative MicroSeconds
    -> NonNegative MicroSeconds
    -> Property
asyncExceptionShouldntAbortExecution std (getNonNegative -> t1) (getNonNegative -> t2) =
    monadicIO $ do
        var <- liftIO $ newTVarIO (0 :: Int)
        liftIO $ runEmulationMode std delays' $ do
            liftIO $ atomically $ writeTVar var 1
            fork $ do
                wait $ for t2 sec
                throwM $ InputProcessingError "Error"
            wait $ for t1 sec
            liftIO $ atomically $ writeTVar var 2
        res <- liftIO $ readTVarIO var
        assert $ res == 2

-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

-- FIXME: use arbitrary instead of StdGen
delays' :: Delays
delays' = Delays d
  where
    d _ _ = Just <$> getRandomR (10, 1000)
