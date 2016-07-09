{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Convenience functions to launch bank or do high-level operations
-- with it.

module RSCoin.Bank.Launcher
       ( bankWrapperReal
       , launchBankReal
       , launchBank
       , addMintetteIO
       , addExplorerIO
       ) where

import           Control.Monad.Catch   (bracket)
import           Control.Monad.Trans   (liftIO)
import           Data.Acid.Advanced    (update')
import           Data.Functor          (void)
import           Data.Time.Units       (TimeUnit)

import           RSCoin.Core           (Explorer, Mintette, PeriodId, PublicKey,
                                        SecretKey)
import           RSCoin.Timed          (MsgPackRpc, ThreadId, WorkMode, fork,
                                        killThread, runRealModeLocal)

import           RSCoin.Bank.AcidState (AddExplorer (AddExplorer),
                                        AddMintette (AddMintette), State,
                                        closeState, openState)
import           RSCoin.Bank.Server    (serve)
import           RSCoin.Bank.Worker    (runWorkerWithPeriod)

bankWrapperReal :: FilePath -> (State -> MsgPackRpc a) -> IO a
bankWrapperReal storagePath =
    runRealModeLocal .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

launchBankReal :: (TimeUnit t) => t -> FilePath -> SecretKey -> IO ()
launchBankReal periodDelta storagePath sk =
    bankWrapperReal storagePath $ void . launchBank periodDelta sk

launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ThreadId
launchBank periodDelta sk st = do
    workerThread <- fork $ runWorkerWithPeriod periodDelta sk st
    workerThread <$ serve st workerThread restartWorkerAction
  where
    restartWorkerAction tId = do
        killThread tId
        fork $ runWorkerWithPeriod periodDelta sk st

addMintetteIO :: FilePath -> Mintette -> PublicKey -> IO ()
addMintetteIO storagePath m k =
    bankWrapperReal storagePath $ flip update' (AddMintette m k)

addExplorerIO :: FilePath -> Explorer -> PeriodId -> IO ()
addExplorerIO storagePath e pId =
    bankWrapperReal storagePath $ flip update' (AddExplorer e pId)
