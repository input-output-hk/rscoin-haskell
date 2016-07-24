{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Convenience functions to launch bank or do high-level operations
-- with it.

module RSCoin.Bank.Launcher
       ( launchBankReal
       , launchBank
       , addMintetteIO
       , addMintetteInPlace
       , addAddressIO
       , addExplorerIO
       ) where

import           Control.Monad             (when)
import           Control.Monad.Catch       (bracket, throwM)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid.Advanced        (update')
import           Data.IORef                (newIORef)
import           Data.Maybe                (fromJust, isNothing)
import           Data.Time.Units           (TimeUnit)

import           Formatting                (int, sformat, (%))

import           RSCoin.Core               (Address, Explorer, Mintette,
                                            PeriodId, PublicKey, SecretKey,
                                            TxStrategy, defaultLayout', sign)
import           RSCoin.Core.Communication (addPendingMintette,
                                            getBlockchainHeight,
                                            getMintettePeriod)
import           RSCoin.Timed              (MsgPackRpc, PlatformLayout,
                                            WorkMode, fork, fork_, killThread,
                                            runRealMode)

import           RSCoin.Bank.AcidState     (AddAddress (AddAddress),
                                            AddExplorer (AddExplorer),
                                            AddMintette (AddMintette), State,
                                            closeState, openState)
import           RSCoin.Bank.Error         (BankError (BEInconsistentResponse))
import           RSCoin.Bank.Server        (serve)
import           RSCoin.Bank.Worker        (runExplorerWorker,
                                            runWorkerWithPeriod)

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

-- | Add mintette to Bank (send a request signed with bank's sk)
-- Also pings minttete to check that it's compatible
addMintetteIO :: SecretKey -> Mintette -> PublicKey -> IO ()
addMintetteIO sk m k = do
    let proof = sign sk (m, k)
    runRealMode (defaultLayout' "127.0.0.1") $ do
        bankPid <- getBlockchainHeight
        mintettePid <- getMintettePeriod m
        when (isNothing mintettePid) $
            throwM $ BEInconsistentResponse
            "Mintette didn't respond on ping request."
        let mPid = fromJust mintettePid
        when (mPid /= (-1) && mPid > bankPid) $
            throwM $ BEInconsistentResponse $
            sformat ("Mintette had period id " % int %
                     " while bank's is " % int %
                     ". Check out, maybe mintette's state" %
                     " is old & incosistent.")
            mPid bankPid
        addPendingMintette m k proof

-- | Adds mintette directly into bank's state
addMintetteInPlace :: FilePath -> Mintette -> PublicKey -> IO ()
addMintetteInPlace storagePath m k =
    bankWrapperReal (defaultLayout' "127.0.0.1") storagePath $
    flip update' (AddMintette m k)

-- | Add explorer to Bank inside IO Monad.
addExplorerIO :: FilePath -> Explorer -> PeriodId -> IO ()
addExplorerIO storagePath e pId =
    bankWrapperReal (defaultLayout' "127.0.0.1") storagePath $
    flip update' (AddExplorer e pId)
