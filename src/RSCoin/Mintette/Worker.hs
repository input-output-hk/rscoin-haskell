{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( isMEInactive
       , runWorker
       ) where

import           Control.Monad             (unless)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (createCheckpoint, update)

import           Serokell.Util.Exceptions  ()
import           Serokell.Util.Text        (formatSingle')

import           RSCoin.Core               (SecretKey, epochDelta, logError,
                                            mintetteLoggerName)

import           RSCoin.Mintette.Acidic    (FinishEpoch (..))
import           RSCoin.Mintette.AcidState (State)
import           RSCoin.Mintette.Error     (isMEInactive)

import           RSCoin.Timed              (WorkMode, repeatForever, sec, tu)

-- | Start worker which updates state when epoch finishes.
runWorker :: WorkMode m => SecretKey -> State -> m ()
runWorker sk st =
    repeatForever (tu epochDelta) handler $ liftIO $ onEpochFinished sk st
  where
    handler e = do
        unless (isMEInactive e) $
            logError mintetteLoggerName $
            formatSingle'
                "Error was caught by worker, restarting in 2 seconds: {}"
                e
        return $ sec 2

onEpochFinished :: SecretKey -> State -> IO ()
onEpochFinished sk st = do
    update st $ FinishEpoch sk
    createCheckpoint st
