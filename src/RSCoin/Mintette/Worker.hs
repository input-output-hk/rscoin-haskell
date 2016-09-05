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

import           RSCoin.Core               (SecretKey, WorkMode,
                                            defaultEpochDelta, logError)
import           RSCoin.Mintette.Acidic    (FinishEpoch (..))
import           RSCoin.Mintette.AcidState (State, update)
import           RSCoin.Mintette.Error     (isMEInactive)

import           RSCoin.Util.Timed         (repeatForever, tu)

-- | Start worker which updates state when epoch finishes. Default
-- epoch length is used.
runWorker :: WorkMode m => SecretKey -> State -> m ()
runWorker = runWorkerWithDelta defaultEpochDelta

-- | Start worker which updates state when epoch finishes. Epoch
-- length is passed as argument.
runWorkerWithDelta
    :: (Show t, Num t, Integral t, TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ()
runWorkerWithDelta epochDelta sk st =
    repeatForever (tu epochDelta) handler $
    liftIO $ onEpochFinished sk st
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

onEpochFinished :: SecretKey -> State -> IO ()
onEpochFinished sk st = update st $ FinishEpoch sk
