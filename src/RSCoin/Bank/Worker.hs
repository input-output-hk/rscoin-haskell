{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module RSCoin.Bank.Worker
       ( runWorker
       , runWorkerWithPeriod
       , runExplorerWorker
       ) where


import           Control.Concurrent.MVar  (MVar, putMVar, takeMVar)
import           Control.Exception        (SomeException)
import           Control.Monad            (forM_)
import           Control.Monad.Catch      (bracket_, catch)
import           Control.Monad.Trans      (MonadIO (liftIO))
import           Data.Acid                (createCheckpoint)
import           Data.Acid.Advanced       (query', update')
import           Data.IORef               (modifyIORef, newIORef, readIORef)
import           Data.List                (sortOn)
import           Data.Maybe               (fromMaybe)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.Time.Units          (TimeUnit)
import           Formatting               (build, int, sformat, (%))

import           Serokell.Util.Bench      (measureTime_)
import           Serokell.Util.Exceptions ()
import           Serokell.Util.Text       (formatSingle')

import           RSCoin.Bank.AcidState    (AddAddress (..),
                                           GetExplorersAndPeriods (..),
                                           GetHBlock (..), GetHBlocks (..),
                                           GetMintettes (..), GetPeriodId (..),
                                           SetExplorerPeriod (..),
                                           StartNewPeriod (..), State)
import           RSCoin.Core              (defaultPeriodDelta,
                                           formatNewPeriodData,
                                           sendPeriodFinished)
import qualified RSCoin.Core              as C
import           RSCoin.Timed             (WorkMode, for, minute, repeatForever,
                                           sec, tu, wait)

logDebug, logInfo, logWarning, logError
    :: MonadIO m
    => Text -> m ()
logDebug = C.logDebug C.bankLoggerName
logInfo = C.logInfo C.bankLoggerName
logWarning = C.logWarning C.bankLoggerName
logError = C.logError C.bankLoggerName

-- | Start worker which runs appropriate action when a period
-- finishes. Default period length is used.
runWorker :: WorkMode m => MVar () -> C.SecretKey -> State -> m ()
runWorker = runWorkerWithPeriod defaultPeriodDelta

-- | Start worker with provided period. Generalization of 'runWorker'.
-- Semaphore is used as synchornization primitive between this worker
-- and another worker (which communicates with explorers).
-- Semaphore is empty iff this worker is doing something now.
runWorkerWithPeriod
    :: (TimeUnit t, WorkMode m)
    => t -> MVar () -> C.SecretKey -> State -> m ()
runWorkerWithPeriod periodDelta semaphore sk st = do
    repeatForever (tu periodDelta) handler worker
  where
    worker = do
        let br =
                bracket_
                    (liftIO $ takeMVar semaphore)
                    (liftIO $ putMVar semaphore ())
        t <- br $ measureTime_ $ onPeriodFinished sk st
        logInfo $ sformat ("Finishing period took " % build) t
    handler e = do
        logError $
            formatSingle'
                "Error was caught by worker, restarting in 1 minute: {}"
                e
        return $ minute 1

onPeriodFinished :: WorkMode m => C.SecretKey -> State -> m ()
onPeriodFinished sk st = do
    mintettes <- query' st GetMintettes
    pId <- query' st GetPeriodId
    logInfo $ formatSingle' "Period {} has just finished!" pId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    periodResults <- getPeriodResults mintettes pId
    newPeriodData <- update' st $ StartNewPeriod sk periodResults
    liftIO $ createCheckpoint st
    newMintettes <- query' st GetMintettes
    if null newMintettes
        then logWarning "New mintettes list is empty!"
        else do
            mapM_
                (\(m,mId) ->
                      C.announceNewPeriod m (newPeriodData !! mId) `catch`
                      handlerAnnouncePeriodM)
                (zip newMintettes [0 ..])
            logInfo $
                formatSingle'
                    ("Announced new period with this NewPeriodData " <>
                     "(payload is Nothing -- omitted (only in Debug)):\n{}")
                    (formatNewPeriodData False $ head newPeriodData)
            logDebug $
                formatSingle'
                    "Announced new period, sent these newPeriodData's:\n{}"
                    newPeriodData

    initializeMultisignatureAddresses
    announceNewPeriodsToNotary `catch` handlerAnnouncePeriodsN
  where
    -- TODO: catch appropriate exception according to protocol implementation
    handlerAnnouncePeriodM (e :: SomeException) =
        logWarning $
        formatSingle' "Error occurred in communicating with mintette: {}" e
    -- TODO: catch appropriate exception according to protocol implementation
    handlerAnnouncePeriodsN (e :: SomeException) =
        logWarning $
        formatSingle' "Error occurred in communicating with Notary: {}" e
    initializeMultisignatureAddresses = do
        newMSAddresses <- C.queryNotaryCompleteMSAddresses
        forM_ newMSAddresses $ \(msAddr, strategy) -> do
            logInfo $ sformat ("Creating MS address " % build % " with strategy " % build)
                msAddr
                strategy
            update' st $ AddAddress msAddr strategy

        logInfo "Removing new addresses from pool"
        C.removeNotaryCompleteMSAddresses $ map fst newMSAddresses
    announceNewPeriodsToNotary = do
        pId <- C.getNotaryPeriod
        pId' <- query' st GetPeriodId
        C.announceNewPeriodsToNotary pId' =<< query' st (GetHBlocks pId pId')

getPeriodResults
    :: WorkMode m
    => C.Mintettes -> C.PeriodId -> m [Maybe C.PeriodResult]
getPeriodResults mts pId = do
    res <- liftIO $ newIORef []
    mapM_ (f res) mts
    liftIO $ reverse <$> readIORef res
  where
    f res mintette =
        (sendPeriodFinished mintette pId >>=
         liftIO . modifyIORef res . (:) . Just) `catch`
        handler res
    handler res (e :: SomeException) =
        liftIO $
        do logWarning $
               formatSingle'
                   "Error occurred in communicating with mintette {}"
                   e
           modifyIORef res (Nothing :)

-- | Start worker which sends data to explorers.
runExplorerWorker
    :: WorkMode m
    => MVar () -> C.SecretKey -> State -> m ()
runExplorerWorker semaphore sk st =
    foreverSafe $
    do liftIO $ takeMVar semaphore
       liftIO $ putMVar semaphore ()
       blocksNumber <- query' st GetPeriodId
       communicateWithExplorers sk st blocksNumber =<<
           query' st GetExplorersAndPeriods
  where
    foreverSafe action = do
        action `catch` handler
        foreverSafe action
    handler (e :: SomeException) = do
        logError $ sformat ("Error occurred inside ExplorerWorker: " % build) e
        wait $ for 10 sec

communicateWithExplorers
    :: WorkMode m
    => C.SecretKey -> State -> C.PeriodId -> [(C.Explorer, C.PeriodId)] -> m ()
communicateWithExplorers sk st blocksNumber =
    mapM_ (communicateWithExplorer sk st blocksNumber) .
    sortOn (negate . snd) . filter ((/= blocksNumber) . snd)

communicateWithExplorer
    :: WorkMode m
    => C.SecretKey -> State -> C.PeriodId -> (C.Explorer, C.PeriodId) -> m ()
communicateWithExplorer sk st blocksNumber (explorer,expectedPeriod)
  | blocksNumber == expectedPeriod = return ()
  | expectedPeriod >= 0 && expectedPeriod < blocksNumber =
      sendBlockToExplorer sk st explorer expectedPeriod
  | otherwise =
      logWarning $
      sformat
          (build % " expects block with strange PeriodId (" % int % ")")
          explorer
          expectedPeriod

sendBlockToExplorer
    :: WorkMode m
    => C.SecretKey -> State -> C.Explorer -> C.PeriodId -> m ()
sendBlockToExplorer sk st explorer pId = do
    blk <- fromMaybe reportFatalError <$> query' st (GetHBlock pId)
    let sendAndUpdate = do
            newExpectedPeriod <-
                C.announceNewBlock explorer pId blk (C.sign sk (pId, blk))
            update' st $ SetExplorerPeriod explorer newExpectedPeriod
    sendAndUpdate `catch` handler
  where
    reportFatalError =
        error "[FATAL] GetHBlock returned Nothing in sendBlockToExplorer"
    -- TODO: catch appropriate exception according to protocol implementation
    handler (e :: SomeException) =
        logWarning .
        sformat ("Error occurred in communicating with explorer: " % build) $
        e
