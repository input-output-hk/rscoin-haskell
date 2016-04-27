{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( runWorker
       ) where

import           Control.Concurrent        (forkFinally, threadDelay)
import           Control.Exception         (SomeException, fromException)
import           Control.Monad             (unless, void)
import           Control.Monad.Catch       (catch)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (createCheckpoint, update)
import           Data.Time.Units           (toMicroseconds)

import           Serokell.Util.Exceptions  ()
import           Serokell.Util.Text        (formatSingle')

import           RSCoin.Core               (SecretKey, epochDelta, logError)

import           RSCoin.Mintette.AcidState (FinishEpoch (..), State)
import           RSCoin.Mintette.Error     (MintetteError (MEInactive))

import           RSCoin.Test               (WorkMode, for, wait, tu, fork, sec)

-- | Start worker which updates state when epoch finishes.
runWorker :: WorkMode m => SecretKey -> State -> m ()
runWorker sk st =
    foreverE $
    do liftIO $ onEpochFinished sk st
       wait $ for epochDelta tu
  where
    foreverE f = let cont = foreverE f
                 in  fork $ (f >> cont) `catch` handler cont
    handler cont (e :: SomeException) = do
        unless (isMEInactive e) $
            liftIO $
            logError $
            formatSingle'
                "Error was caught by worker, restarting in 2 seconds: {}"
                e
        wait $ for 2 sec
        cont
 
isMEInactive :: SomeException -> Bool
isMEInactive = maybe False (== MEInactive) . fromException

onEpochFinished :: SecretKey -> State -> IO ()
onEpochFinished sk st = do
    update st $ FinishEpoch sk
    createCheckpoint st
