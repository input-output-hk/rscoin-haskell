{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( runWorker
       ) where

import           Control.Exception         (SomeException, fromException)
import           Control.Monad             (unless)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (createCheckpoint, update)

import           Serokell.Util.Exceptions  ()
import           Serokell.Util.Text        (formatSingle')

import           RSCoin.Core               (SecretKey, epochDelta, logError)

import           RSCoin.Mintette.AcidState (FinishEpoch (..), State)
import           RSCoin.Mintette.Error     (MintetteError (MEInactive))

import           RSCoin.Timed              (WorkMode, tu, sec,
                                            repeatForever)

-- | Start worker which updates state when epoch finishes.
runWorker :: WorkMode m => SecretKey -> State -> m ()
runWorker sk st = repeatForever (tu epochDelta) handler $ 
    liftIO $ onEpochFinished sk st
  where
    handler e = do
        unless (isMEInactive e) $
            liftIO $
            logError $
            formatSingle'
                "Error was caught by worker, restarting in 2 seconds: {}"
                e
        return $ sec 2
 
isMEInactive :: SomeException -> Bool
isMEInactive = maybe False (== MEInactive) . fromException

onEpochFinished :: SecretKey -> State -> IO ()
onEpochFinished sk st = do
    update st $ FinishEpoch sk
    createCheckpoint st
