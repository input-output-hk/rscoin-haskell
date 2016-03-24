{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that handles end of period.

module Worker
       ( runWorker
       ) where

import           Control.Concurrent    (forkFinally, threadDelay)
import           Control.Exception     (SomeException)
import           Control.Monad         (void)
import           Data.Time.Units       (toMicroseconds)

import           RSCoin.Core.Constants (periodDelta)

import           AcidState             (State)

-- | Start worker which runs appropriate action when a period finishes
runWorker :: State -> IO ()
runWorker st =
    foreverE $
    do threadDelay (fromIntegral $ toMicroseconds periodDelta)
       onPeriodFinished st
  where
    foreverE f = void $ forkFinally f $ handler $ foreverE f
    -- TODO: use logging system once we have one
    handler f (Left (e :: SomeException)) = do
        putStrLn $ "Error occurred in worker, restarting in 1 minute: " ++ show e
        f
    handler f (Right _) = f

onPeriodFinished :: State -> IO ()
onPeriodFinished = undefined
