{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module RSCoin.Bank.Worker
       ( runWorker
       , runWorkerWithPeriod
       ) where


import           Control.Exception        (SomeException)
import           Control.Monad            (forM_)
import           Control.Monad.Catch      (catch)
import           Control.Monad.Trans      (MonadIO (liftIO))

import           Data.Acid                (createCheckpoint)
import           Data.Acid.Advanced       (query', update')
import           Data.IORef               (modifyIORef, newIORef, readIORef)
import           Data.Maybe               (fromJust)
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.Time.Units          (TimeUnit)
import           Formatting               (build, sformat, (%))

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
import           RSCoin.Timed             (WorkMode, minute, repeatForever, tu)

logDebug :: MonadIO m => Text -> m ()
logDebug = C.logDebug C.bankLoggerName

logInfo :: MonadIO m => Text -> m ()
logInfo = C.logInfo C.bankLoggerName

logWarning :: MonadIO m => Text -> m ()
logWarning = C.logWarning C.bankLoggerName

logError :: MonadIO m => Text -> m ()
logError = C.logError C.bankLoggerName

-- | Start worker which runs appropriate action when a period
-- finishes. Default period length is used.
runWorker :: WorkMode m => C.SecretKey -> State -> m ()
runWorker = runWorkerWithPeriod defaultPeriodDelta

-- | Start worker with provided period. Generalization of 'runWorker'.
runWorkerWithPeriod :: (TimeUnit t, WorkMode m) => t -> C.SecretKey -> State -> m ()
runWorkerWithPeriod periodDelta sk st = repeatForever (tu periodDelta) handler $ do
    t <- measureTime_ $ onPeriodFinished sk st
    logInfo $ sformat ("Finishing period took " % build) t
  where
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

    communicateWithExplorers sk st =<< query' st GetExplorersAndPeriods
    initializeMultisignatureAddresses
    announceNewPeriodsToNotary `catch` handlerAnnouncePeriodsS
  where
    -- TODO: catch appropriate exception according to protocol
    -- implementation (here and below)
    handlerAnnouncePeriodM (e :: SomeException) =
        logWarning $
        formatSingle' "Error occurred in communicating with mintette: {}" e
    handlerAnnouncePeriodsS (e :: SomeException) =
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

-- TODO: this communication must be more smart to prevent bank from
-- spending too much time on communication with particular
-- explorer. And there are more things to improve. For instance, error
-- handling.
communicateWithExplorers
    :: WorkMode m
    => C.SecretKey -> State -> [(C.Explorer, C.PeriodId)] -> m ()
communicateWithExplorers sk st = mapM_ (communicateWithExplorer sk st)

communicateWithExplorer
    :: WorkMode m
    => C.SecretKey -> State -> (C.Explorer, C.PeriodId) -> m ()
communicateWithExplorer sk st (explorer,expectedPeriod) = do
    latest <- pred <$> query' st GetPeriodId
    newExpectedPeriod <-
        last <$>
        mapM
            (sendBlockToExplorer sk st explorer)
            [max 0 expectedPeriod .. latest]
    update' st $ SetExplorerPeriod explorer newExpectedPeriod

sendBlockToExplorer
    :: WorkMode m
    => C.SecretKey -> State -> C.Explorer -> C.PeriodId -> m C.PeriodId
sendBlockToExplorer sk st explorer pId = do
    blk <- fromJust <$> query' st (GetHBlock pId)
    -- TODO: check sig!
    fst <$> C.announceNewBlock explorer pId blk (C.sign sk (pId, blk))
