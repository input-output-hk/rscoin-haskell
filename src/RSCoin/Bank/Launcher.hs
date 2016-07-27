{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Convenience functions to launch bank or do high-level operations
-- with it.

module RSCoin.Bank.Launcher
       ( launchBankReal
       , launchBank
       , addMintetteIO
       , addMintetteInPlace
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

import           RSCoin.Core               (Explorer, Mintette, PeriodId,
                                            PublicKey, SecretKey, sign)
import           RSCoin.Core.Communication (addPendingMintette,
                                            getBlockchainHeight,
                                            getMintettePeriod)
import           RSCoin.Timed              (MsgPackRpc, WorkMode, fork, fork_,
                                            killThread, runRealModeBank)

import           RSCoin.Bank.AcidState     (AddExplorer (AddExplorer),
                                            AddMintette (AddMintette), State,
                                            closeState, openState)
import           RSCoin.Bank.Error         (BankError (BEInconsistentResponse))
import           RSCoin.Bank.Server        (serve)
import           RSCoin.Bank.Worker        (runExplorerWorker,
                                            runWorkerWithPeriod)

bankWrapperReal :: SecretKey -> FilePath -> (State -> MsgPackRpc a) -> IO a
bankWrapperReal bankSk storagePath =
    runRealModeBank bankSk .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

-- | Launch Bank in real mode. This function works indefinitely.
launchBankReal
    :: (TimeUnit t)
    => t -> FilePath -> SecretKey -> IO ()
launchBankReal periodDelta storagePath bankSk =
    bankWrapperReal bankSk storagePath $ launchBank periodDelta bankSk

-- | Launch Bank in any WorkMode. This function works indefinitely.
launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ()
launchBank periodDelta bankSk st = do
    mainIsBusy <- liftIO $ newIORef False
    let startWorker = runWorkerWithPeriod periodDelta mainIsBusy bankSk st
        restartWorker tId = killThread tId >> fork startWorker
    workerThread <- fork startWorker
    fork_ $ runExplorerWorker periodDelta mainIsBusy bankSk st
    serve st workerThread restartWorker

-- | Add mintette to Bank (send a request signed with bank's sk)
-- Also pings minttete to check that it's compatible
addMintetteIO :: SecretKey -> Mintette -> PublicKey -> IO ()
addMintetteIO bankSk m k = do
    let proof = sign bankSk (m, k)
    runRealModeBank bankSk $ do  -- @TODO: why not 'bankWrapperReal' here? Is it remote call?
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
addMintetteInPlace :: SecretKey -> FilePath -> Mintette -> PublicKey -> IO ()
addMintetteInPlace bankSk storagePath m k =
    bankWrapperReal bankSk storagePath $
    flip update' (AddMintette m k)

-- | Add explorer to Bank inside IO Monad.
addExplorerIO :: SecretKey -> FilePath -> Explorer -> PeriodId -> IO ()
addExplorerIO bankSk storagePath e pId =
    bankWrapperReal bankSk storagePath $
    flip update' (AddExplorer e pId)
