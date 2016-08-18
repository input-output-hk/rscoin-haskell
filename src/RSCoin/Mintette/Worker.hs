{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( runWorker
       , runWorkerWithDelta
       ) where

import           Control.Monad             (unless)
import           Control.Monad.Extra       (whenJust)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (createCheckpoint, update)
import           Data.Time.Units           (TimeUnit)
import           Formatting                (build, sformat, (%))

import           Serokell.Util.AcidState   (createAndDiscardArchive)

import           RSCoin.Core               (SecretKey, defaultEpochDelta,
                                            logError)
import           RSCoin.Mintette.Acidic    (FinishEpoch (..))
import           RSCoin.Mintette.AcidState (State)
import           RSCoin.Mintette.Error     (isMEInactive)

import           RSCoin.Timed              (WorkMode, repeatForever, sec, tu)

-- | Start worker which updates state when epoch finishes. Default
-- epoch length is used.
runWorker :: WorkMode m => SecretKey -> State -> Maybe FilePath -> m ()
runWorker = runWorkerWithDelta defaultEpochDelta

-- | Start worker which updates state when epoch finishes. Epoch
-- length is passed as argument.
runWorkerWithDelta
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> Maybe FilePath -> m ()
runWorkerWithDelta epochDelta sk st storagePath =
    repeatForever (tu epochDelta) handler $
    liftIO $ onEpochFinished sk st storagePath
  where
    handler e = do
        unless (isMEInactive e) $
            logError $
            sformat
                ("Error was caught by worker, restarting in 2 seconds: " % build) e
        return $ sec 2

onEpochFinished :: SecretKey -> State -> Maybe FilePath -> IO ()
onEpochFinished sk st storagePath = do
    update st $ FinishEpoch sk
    createCheckpoint st
    --pid <- query st GetPeriodId -- can be used to cleanup archive once in N periods
    whenJust storagePath $ createAndDiscardArchive st
