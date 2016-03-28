{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module RSCoin.Bank.Worker
       ( runWorker
       ) where

import           Control.Concurrent    (forkFinally, threadDelay)
import           Control.Exception     (SomeException, try)
import           Control.Monad         (void)
import           Data.Acid             (createCheckpoint, query, update)
import           Data.Time.Units       (toMicroseconds)

import           RSCoin.Core           (Mintette (..), NewPeriodData, PeriodId,
                                        PeriodResult, SecretKey, periodDelta,
                                        call, MintetteReq (..), MintetteRes (..))

import           RSCoin.Bank.AcidState (GetMintettes (..), GetPeriodId (..),
                                        StartNewPeriod (..), State)

-- | Start worker which runs appropriate action when a period finishes
runWorker :: SecretKey -> State -> IO ()
runWorker sk st =
    foreverE $
    do onPeriodFinished sk st
       threadDelay (fromIntegral $ toMicroseconds periodDelta)
  where
    foreverE f = void $ forkFinally f $ handler $ foreverE f
    -- TODO: use logging system once we have one
    handler f (Left (e :: SomeException)) = do
        putStrLn $
            "Error occurred in worker, restarting in 1 minute: " ++ show e
        threadDelay $ 1 * 60 * 1000 * 1000
        f
    handler f (Right _) = f

onPeriodFinished :: SecretKey -> State -> IO ()
onPeriodFinished sk st = do
    mintettes <- query st GetMintettes
    pId <- query st GetPeriodId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    periodResults <- mapM (handlerPeriodFinished . flip sendPeriodFinished pId) mintettes
    newPeriodData <- update st $ StartNewPeriod sk periodResults
    createCheckpoint st
    newMintettes <- query st GetMintettes
    mapM_ (handlerAnnouncePeriod . flip announceNewPeriod newPeriodData) newMintettes
  where
    handlerPeriodFinished action = do
        either onPeriodFinishedError (return . Just) =<< try action
    -- TODO: catching appropriate exception according to protocol implementation
    onPeriodFinishedError (e :: SomeException) = do
        putStrLn $ "Error occurred: " ++ show e
        return Nothing
    handlerAnnouncePeriod = id  -- TODO

sendPeriodFinished :: Mintette -> PeriodId -> IO PeriodResult
sendPeriodFinished Mintette {..} =
    fmap fromResponse . call mintettePort mintetteHost . ReqPeriodFinished
    where fromResponse (ResPeriodFinished pr) = pr
          fromResponse _ = error "SendPeriodFinished got unexpected result"

announceNewPeriod :: Mintette -> NewPeriodData -> IO ()
announceNewPeriod Mintette {..} = 
    fmap fromResponse . call mintettePort mintetteHost . ReqAnnounceNewPeriod
    where fromResponse ResAnnounceNewPeriod = ()
          fromResponse _ = error "AnnounceNewPeriod got unexpected result"
