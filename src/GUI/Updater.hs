module Updater where

import Control.Concurrent
import Control.Concurrent.STM.TBQueue

import Control.Monad.STM

import AcidExecutor

runUpdater :: TBQueue Operation -> IO ()
runUpdater queue = do
    atomically $ writeTBQueue queue Update
    threadDelay 30000
    runUpdater queue
