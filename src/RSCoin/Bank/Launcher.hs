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
       , permitMintetteReq
       , addExplorerReq
       , removeMintetteReq
       , removeExplorerReq
       , dumpStatisticsReq
       ) where

import           Control.Monad             (when)
import           Control.Monad.Catch       (bracket, throwM)
import           Control.Monad.Trans       (liftIO)
import           Data.IORef                (newIORef)
import           Data.Maybe                (fromJust, isNothing)
import           Data.Time.Units           (TimeUnit)
import           Formatting                (int, sformat, (%))

import           Control.TimeWarp.Timed    (for, fork_, ms, wait)
import           RSCoin.Core               (ContextArgument (..), Explorer (..),
                                            Mintette, PeriodId, PublicKey,
                                            RealMode, SecretKey, WorkMode,
                                            runRealModeBank, sign)
import           RSCoin.Core.Communication (getBlockchainHeight,
                                            getMintettePeriod, getStatisticsId,
                                            sendBankLocalControlRequest)
import qualified RSCoin.Core.Protocol      as P (BankLocalControlRequest (..))

import           RSCoin.Bank.AcidState     (AddExplorer (AddExplorer),
                                            PermitMintette (PermitMintette),
                                            AddMintette (AddMintette), State,
                                            closeState, openState, update)
import           RSCoin.Bank.Error         (BankError (BEBadRequest))
import           RSCoin.Bank.Server        (serve)
import           RSCoin.Bank.Worker        (runExplorerWorker, runWorker)

bankWrapperReal :: SecretKey
                -> Bool
                -> FilePath
                -> ContextArgument
                -> (State -> RealMode a)
                -> IO a
bankWrapperReal bankSk deleteIfExists storagePath ca =
    runRealModeBank ca bankSk .
    bracket (liftIO $ openState deleteIfExists storagePath) (liftIO . closeState)

-- | Launch Bank in real mode. This function works indefinitely.
launchBankReal
    :: (TimeUnit t)
    => Bool -> t -> FilePath -> ContextArgument -> SecretKey -> IO ()
launchBankReal deleteIfExists periodDelta storagePath ca bankSk =
    bankWrapperReal bankSk deleteIfExists storagePath ca $
    launchBank periodDelta bankSk

-- | Launch Bank in any WorkMode. This function works indefinitely.
launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ()
launchBank periodDelta bankSk st = do
    isPeriodChanging <- liftIO $ newIORef False
    fork_ $ serve st bankSk isPeriodChanging
    wait $ for 1 ms
    fork_ $ runWorker periodDelta bankSk st
    runExplorerWorker isPeriodChanging bankSk st

-- | Adds mintette directly into bank's state
addMintetteInPlace :: ContextArgument
                   -> SecretKey
                   -> FilePath
                   -> Mintette
                   -> PublicKey
                   -> IO ()
addMintetteInPlace ca bankSk storagePath m k =
    bankWrapperReal bankSk False storagePath ca $ flip update (AddMintette m k)

-- | Add explorer to Bank inside IO Monad.
addExplorerInPlace :: ContextArgument
                   -> SecretKey
                   -> FilePath
                   -> Explorer
                   -> PeriodId
                   -> IO ()
addExplorerInPlace ca bankSk storagePath e pId =
    bankWrapperReal bankSk False storagePath ca $ flip update (AddExplorer e pId)

wrapRequest :: ContextArgument -> SecretKey -> P.BankLocalControlRequest -> IO ()
wrapRequest ca bankSk request =
    runRealModeBank ca bankSk $ sendBankLocalControlRequest request

-- | Add mintette to Bank (send a request signed with bank's sk)
-- Also pings minttete to check that it's compatible
addMintetteReq :: ContextArgument -> SecretKey -> Mintette -> PublicKey -> IO ()
addMintetteReq ca bankSk m k = do
    let proof = sign bankSk (m, k)
    runRealModeBank ca bankSk $ do
        bankPid <- getBlockchainHeight
        mintettePid <- getMintettePeriod m
        when (isNothing mintettePid) $
            throwM $ BEBadRequest
            "Mintette didn't respond on ping request."
        let mPid = fromJust mintettePid
        when (mPid /= (-1) && mPid > bankPid) $
            throwM $ BEBadRequest $
            sformat ("Mintette had period id " % int %
                     " while bank's is " % int %
                     ". Check out, maybe mintette's state" %
                     " is old & incosistent.")
            mPid bankPid
        sendBankLocalControlRequest (P.AddMintette m k proof)

-- | Permit mintette's owner public key for later addition
permitMintetteReq :: ContextArgument -> SecretKey -> PublicKey -> IO ()
permitMintetteReq ca bankSk pk = do
    let proof = sign bankSk pk
    wrapRequest ca bankSk $ P.PermitMintette pk proof

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

-- | Sends a request to remove explorer.
removeExplorerReq :: ContextArgument  -> SecretKey -> String -> Int -> IO ()
removeExplorerReq ca bankSk explorerHost explorerPort = do
    let proof = sign bankSk (explorerHost, explorerPort)
    wrapRequest ca bankSk $ P.RemoveExplorer explorerHost explorerPort proof

-- | Sends a request to dump statistics about database usage.
dumpStatisticsReq :: ContextArgument -> SecretKey -> IO ()
dumpStatisticsReq ca bankSk = do
    sId <- runRealModeBank ca bankSk getStatisticsId
    let proof = sign bankSk sId
    wrapRequest ca bankSk $ P.DumpStatistics sId proof
