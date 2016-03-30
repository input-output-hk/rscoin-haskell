{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( runWorker
       ) where

import           Control.Concurrent        (forkFinally, threadDelay)
import           Control.Exception         (SomeException)
import           Control.Monad             (void)
import           Data.Acid                 (createCheckpoint, update)
import           Data.Time.Units           (toMicroseconds)

import           Serokell.Util.Exceptions  ()
import           Serokell.Util.Text        (formatSingle')

import           RSCoin.Core               (SecretKey, epochDelta, logError)

import           RSCoin.Mintette.AcidState (FinishEpoch (..), State)

-- | Start worker which updates state when epoch finishes.
runWorker :: SecretKey -> State -> IO ()
runWorker sk st =
    foreverE $
    do onEpochFinished sk st
       threadDelay (fromIntegral $ toMicroseconds epochDelta)
  where
    foreverE f = void $ forkFinally f $ handler $ foreverE f
    handler f (Left (e :: SomeException)) = do
        logError $
            formatSingle'
                "Error was caught by worker, restarting in 2 seconds: {}"
                e
        threadDelay $ 2 * 1000 * 1000
        f
    handler f (Right _) = f

onEpochFinished :: SecretKey -> State -> IO ()
onEpochFinished sk st = do
    update st $ FinishEpoch sk
    createCheckpoint st