{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Convenience functions to launch bank or do high-level operations
-- with it.

module RSCoin.Bank.Launcher
       ( ContextArgument (..)
       , launchBankReal
       , launchBank
       , addMintetteInPlace
       , addExplorerInPlace
       , addMintetteReq
       , addExplorerReq
       , removeMintetteReq
       , removeExplorerReq
       ) where

import           Control.Monad             (when)
import           Control.Monad.Catch       (bracket, throwM)
import           Control.Monad.Extra       (whenJust)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid.Advanced        (update')
import           Data.IORef                (newIORef)
import           Data.Maybe                (fromJust, isNothing)
import           Data.Time.Units           (TimeUnit)
import           Formatting                (int, sformat, (%))

import           Serokell.Util.Text        (show')

import           RSCoin.Core               (Explorer, Mintette, PeriodId,
                                            PublicKey, SecretKey, logError,
                                            sign)
import           RSCoin.Core.Communication (getBlockchainHeight,
                                            getMintettePeriod,
                                            sendBankLocalControlRequest)
import qualified RSCoin.Core.Protocol      as P (BankLocalControlRequest (..))
import           RSCoin.Timed              (ContextArgument (..), MsgPackRpc,
                                            WorkMode, fork, fork_, killThread,
                                            runRealModeBank)

import           RSCoin.Bank.AcidState     (AddExplorer (AddExplorer),
                                            AddMintette (AddMintette), State,
                                            closeState, openState)
import           RSCoin.Bank.Error         (BankError (BEInconsistentResponse))
import           RSCoin.Bank.Server        (serve)
import           RSCoin.Bank.Worker        (runExplorerWorker,
                                            runWorkerWithPeriod)

bankWrapperReal :: SecretKey
                -> FilePath
                -> ContextArgument
                -> (State -> MsgPackRpc a)
                -> IO a
bankWrapperReal bankSk storagePath ca =
    runRealModeBank ca bankSk .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

-- | Launch Bank in real mode. This function works indefinitely.
launchBankReal
    :: (TimeUnit t)
    => t -> FilePath -> ContextArgument -> SecretKey -> IO ()
launchBankReal periodDelta storagePath ca bankSk =
    bankWrapperReal bankSk storagePath ca $
    launchBank periodDelta bankSk storagePath

-- | Launch Bank in any WorkMode. This function works indefinitely.
launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> FilePath -> State -> m ()
launchBank periodDelta bankSk storagePath st = do
    mainIsBusy <- liftIO $ newIORef False
    let startWorker =
            runWorkerWithPeriod
                periodDelta
                mainIsBusy
                bankSk
                st
                (Just storagePath)
        restartWorker tId = killThread tId >> fork startWorker
    workerThread <- fork startWorker
    fork_ $ runExplorerWorker periodDelta mainIsBusy bankSk st
    serve st workerThread restartWorker

-- | Adds mintette directly into bank's state
addMintetteInPlace :: ContextArgument
                   -> SecretKey
                   -> FilePath
                   -> Mintette
                   -> PublicKey
                   -> IO ()
addMintetteInPlace ca bankSk storagePath m k =
    bankWrapperReal bankSk storagePath ca $ flip update' (AddMintette m k)

-- | Add explorer to Bank inside IO Monad.
addExplorerInPlace :: ContextArgument
                   -> SecretKey
                   -> FilePath
                   -> Explorer
                   -> PeriodId
                   -> IO ()
addExplorerInPlace ca bankSk storagePath e pId =
    bankWrapperReal bankSk storagePath ca $ flip update' (AddExplorer e pId)

wrapResult res = whenJust res $ \e -> logError (show' e) >> throwM e

wrapRequest :: ContextArgument -> SecretKey -> P.BankLocalControlRequest -> IO ()
wrapRequest ca bankSk request = do
    res <- runRealModeBank ca bankSk $ sendBankLocalControlRequest request
    wrapResult res

-- | Add mintette to Bank (send a request signed with bank's sk)
-- Also pings minttete to check that it's compatible
addMintetteReq :: ContextArgument -> SecretKey -> Mintette -> PublicKey -> IO ()
addMintetteReq ca bankSk m k = do
    let proof = sign bankSk (m, k)
    runRealModeBank ca bankSk $ do
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
        wrapResult =<< sendBankLocalControlRequest (P.AddMintette m k proof)

-- | Add explorer to Bank inside IO Monad.
addExplorerReq :: ContextArgument -> SecretKey -> Explorer -> PeriodId -> IO ()
addExplorerReq ca bankSk e pId = do
    let proof = sign bankSk (e, pId)
    wrapRequest ca bankSk $ P.AddExplorer e pId proof

-- | Sends a request to remove mintette
removeMintetteReq :: ContextArgument -> SecretKey -> String -> Int -> IO ()
removeMintetteReq ca bankSk mintetteHost mintettePort = do
    let proof = sign bankSk (mintetteHost, mintettePort)
    wrapRequest ca bankSk $ P.RemoveMintette mintetteHost mintettePort proof

-- | Sends a request to remove explorer
removeExplorerReq :: ContextArgument  -> SecretKey -> String -> Int -> IO ()
removeExplorerReq ca bankSk explorerHost explorerPort = do
    let proof = sign bankSk (explorerHost, explorerPort)
    wrapRequest ca bankSk $ P.RemoveExplorer explorerHost explorerPort proof
