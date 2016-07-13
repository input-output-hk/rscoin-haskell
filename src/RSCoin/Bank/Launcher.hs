{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Convenience functions to launch bank or do high-level operations
-- with it.

module RSCoin.Bank.Launcher
       ( launchBankReal
       , launchBank
       , addMintetteIO
       , addAddressIO
       , addExplorerIO
       ) where

import           Control.Monad.Catch   (bracket)
import           Control.Monad.Trans   (liftIO)
import           Data.Acid.Advanced    (update')
import           Data.IORef            (newIORef)
import           Data.Time.Units       (TimeUnit)


import           RSCoin.Core           (Address, Explorer, Mintette, PeriodId, PublicKey,
                                        SecretKey, TxStrategy, defaultLayout')
import           RSCoin.Timed          (MsgPackRpc, PlatformLayout, WorkMode,
                                        fork, fork_, killThread, runRealMode)

import           RSCoin.Bank.AcidState (AddAddress (AddAddress),
                                        AddExplorer (AddExplorer),
                                        AddMintette (AddMintette), State,
                                        closeState, openState)
import           RSCoin.Bank.Server    (serve)
import           RSCoin.Bank.Worker    (runExplorerWorker, runWorkerWithPeriod)

bankWrapperReal :: PlatformLayout -> FilePath -> (State -> MsgPackRpc a) -> IO a
bankWrapperReal layout storagePath =
    runRealMode layout .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

-- | Launch Bank in real mode. This function works indefinitely.
launchBankReal
    :: (TimeUnit t)
    => PlatformLayout -> t -> FilePath -> SecretKey -> IO ()
launchBankReal layout periodDelta storagePath sk =
    bankWrapperReal layout storagePath $ launchBank periodDelta sk

-- | Launch Bank in any WorkMode. This function works indefinitely.
launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ()
launchBank periodDelta sk st = do
    mainIsBusy <- liftIO $ newIORef False
    let startWorker = runWorkerWithPeriod periodDelta mainIsBusy sk st
        restartWorker tId = killThread tId >> fork startWorker
    workerThread <- fork startWorker
    fork_ $ runExplorerWorker periodDelta mainIsBusy sk st
    serve st workerThread restartWorker

addAddressIO :: FilePath -> Address -> TxStrategy -> IO ()
addAddressIO storagePath a s =
    bankWrapperReal (defaultLayout' "127.0.0.1") storagePath $ flip update' (AddAddress a s)

-- | Add mintette to Bank inside IO Monad.
addMintetteIO :: FilePath -> Mintette -> PublicKey -> IO ()
addMintetteIO storagePath m k =
    bankWrapperReal (defaultLayout' "127.0.0.1") storagePath $
    flip update' (AddMintette m k)

-- | Add explorer to Bank inside IO Monad.
addExplorerIO :: FilePath -> Explorer -> PeriodId -> IO ()
addExplorerIO storagePath e pId =
    bankWrapperReal (defaultLayout' "127.0.0.1") storagePath $
    flip update' (AddExplorer e pId)
