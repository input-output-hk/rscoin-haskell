{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( runWorker
       , runWorkerWithDelta
       ) where

import           Control.Monad             (unless)
import           Control.Monad.Trans       (liftIO)
import           Data.Time.Units           (TimeUnit)
import           Formatting                (build, sformat, shown, (%))

import           Control.TimeWarp.Timed    (repeatForever, tu)

import           RSCoin.Core               (WorkMode, defaultEpochDelta,
                                            logError)
import           RSCoin.Mintette.Acidic    (FinishEpoch (..))
import           RSCoin.Mintette.AcidState (State, update)
import           RSCoin.Mintette.Env       (RuntimeEnv)
import           RSCoin.Mintette.Error     (isMEInactive)

-- | Start worker which updates state when epoch finishes. Default
-- epoch length is used.
runWorker :: WorkMode m => RuntimeEnv -> State -> m ()
runWorker = runWorkerWithDelta defaultEpochDelta

-- | Start worker which updates state when epoch finishes. Epoch
-- length is passed as argument.
runWorkerWithDelta
    :: (Show t, Num t, Integral t, TimeUnit t, WorkMode m)
    => t -> RuntimeEnv -> State -> m ()
runWorkerWithDelta epochDelta env st =
    repeatForever (tu epochDelta) handler $
    liftIO $ onEpochFinished env st
  where
    restartDelay = epochDelta `div` 3
    handler e = do
        unless (isMEInactive e) $
            logError $
            sformat
                ("Error was caught by worker, restarting in " % shown % ": " %
                 build)
                restartDelay
                e
        return $ tu restartDelay

onEpochFinished :: RuntimeEnv -> State -> IO ()
onEpochFinished env st = update st $ FinishEpoch env
