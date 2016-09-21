{-# LANGUAGE GADTs #-}

module Test.RSCoin.Full.Threads
    ( makeThreadsController
    ) where

import           Control.Concurrent.STM            (atomically)
import           Control.Concurrent.STM.TVar       (newTVarIO, readTVar,
                                                    readTVarIO, modifyTVar,
                                                    swapTVar)
import           Control.Exception                 (Exception)
import           Control.Monad                     (when, forM_)
import           Control.Monad.Trans               (liftIO)
import           Data.Maybe                        (isJust, isNothing)

import           Control.TimeWarp.Timed.MonadTimed (MonadTimed (fork, throwTo))
import           RSCoin.Core                       (WorkMode)

-- | Such a pathetic name :)
-- Creates a pair of @fork@ and @killAll@ functions, where @fork@ creates
-- a new thread, and @killAll@ arises specified exception in all
-- threads produced by @fork@ and prevents @fork@ from creating new ones.
makeThreadsController ::
    (WorkMode m, Exception e) =>
    e -> m (m () -> m (), m ())
makeThreadsController e = do
    -- keeps @Just list@ of created threads, or @Nothing@ if `killAll`
    -- has been invoked
    threadIds <- liftIO . newTVarIO $ Just []
    let forkTmp action = do
            wasThreadIds <- liftIO $ readTVarIO threadIds
            when (isJust wasThreadIds) $ do
                tid <- fork action
                liftIO . atomically $ modifyTVar threadIds $ fmap $ (:) tid
                curThreadIds <- liftIO . atomically $ readTVar threadIds
                when (isNothing curThreadIds) $
                    throwTo tid e
    let killAll = do
            maybeTids <- liftIO . atomically $ swapTVar threadIds Nothing
            case maybeTids of
                Nothing   -> error "Already killed!"
                Just tids -> forM_ tids $ flip throwTo e
    return (forkTmp, killAll)
