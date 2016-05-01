{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module RSCoin.Bank.Worker
       ( runWorker
       ) where


import           Control.Exception        (SomeException)
import           Control.Monad.Catch      (catch)
import           Control.Monad.Trans      (liftIO)
import           Data.Acid                (createCheckpoint, query, update)
import           Data.IORef               (modifyIORef, newIORef, readIORef)
import           Data.Monoid              ((<>))

import           Serokell.Util.Exceptions ()
import           Serokell.Util.Text       (formatSingle')

import           RSCoin.Core              (Mintettes, PeriodId, PeriodResult,
                                           SecretKey, announceNewPeriod,
                                           formatNewPeriodData, logDebug,
                                           logError, logInfo, logWarning,
                                           periodDelta, sendPeriodFinished)

import           RSCoin.Test              (WorkMode, minute, tu, repeatForever)
import           RSCoin.Bank.AcidState    (GetMintettes (..), GetPeriodId (..),
                                           StartNewPeriod (..), State)

-- | Start worker which runs appropriate action when a period finishes
runWorker :: WorkMode m => SecretKey -> State -> m ()
runWorker sk st = repeatForever (tu periodDelta) handler $
    onPeriodFinished sk st
  where
    handler e = do
        logError $
            formatSingle'
                "Error was caught by worker, restarting in 1 minute: {}"
                e
        return $ minute 1

onPeriodFinished :: WorkMode m => SecretKey -> State -> m ()
onPeriodFinished sk st = do
    mintettes <- liftIO $ query st GetMintettes
    pId <- liftIO $ query st GetPeriodId
    liftIO $ logInfo $ formatSingle' "Period {} has just finished!" pId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    periodResults <- getPeriodResults mintettes pId
    newPeriodData <- liftIO $ update st $ StartNewPeriod sk periodResults
    liftIO $ createCheckpoint st
    newMintettes <- liftIO $ query st GetMintettes
    if null newMintettes
        then liftIO $ logWarning "New mintettes list is empty!"
        else do
            mapM_
                (\(m,mId) ->
                      announceNewPeriod m (newPeriodData !! mId) `catch`
                      handlerAnnouncePeriod)
                (zip newMintettes [0 ..])
            liftIO $ logInfo $
                formatSingle'
                    ("Announced new period with this NewPeriodData " <>
                     "(payload is Nothing -- omitted (only in Debug)):\n{}")
                    (formatNewPeriodData False $ head newPeriodData)
            liftIO $ logDebug $
                formatSingle'
                   "Announced new period, sent these newPeriodData's:\n{}"
                   newPeriodData
  where
    -- TODO: catch appropriate exception according to protocol
    -- implementation (here and below)
    handlerAnnouncePeriod (e :: SomeException) =
        liftIO $
        logWarning $
        formatSingle' "Error occurred in communicating with mintette {}" e

getPeriodResults :: WorkMode m => Mintettes -> PeriodId -> m [Maybe PeriodResult]
getPeriodResults mts pId = do
    res <- liftIO $ newIORef []
    mapM_ (f res) mts
    liftIO $ reverse <$> readIORef res
  where
    f res mintette =
        (sendPeriodFinished mintette pId 
            >>= liftIO . modifyIORef res . (:) . Just)  
                `catch` handler res
    handler res (e :: SomeException) = liftIO $ do
        logWarning $
            formatSingle' "Error occurred in communicating with mintette {}" e
        modifyIORef res (Nothing :)
