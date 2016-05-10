module RSCoin.Timed.Misc
    where

import Control.Exception.Base      (SomeException)
import Control.Monad.Catch         (MonadCatch, MonadMask, mask, throwM, catch)
import Control.Monad.Trans         (MonadIO, liftIO)
import Control.Monad.STM           (atomically)
import Control.Concurrent.STM.TVar as T

import RSCoin.Timed.MonadTimed     (MonadTimed, MicroSeconds, wait, fork, 
                                    ms, mcs, for, startTimer)

-- | Implementation of bracket for any MonadMask
bracket' :: MonadMask m
         => m a         -- ^ computation to run first (\"acquire resource\")
         -> (a -> m b)  -- ^ computation to run last (\"release resource\")
         -> (a -> m c)  -- ^ computation to run in-between
         -> m c         -- returns the value from the in-between computation
bracket' before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
  where
    onException io what = io `catch` \e -> what >> throwM (e :: SomeException)


-- | Repeats an action periodically. 
--   If it fails, handler is invoked, determing delay for retrying.
--   Can be interrupted with asyncronious exception.
repeatForever :: (MonadTimed m, MonadIO m, MonadCatch m) 
              => MicroSeconds    -- ^ Period between action launches
              -> (SomeException -> m MicroSeconds)  
                                 -- ^ What to do on exception,  
                                 --   returns delay before retrying
              -> m ()            -- ^ Action
              -> m ()           
repeatForever period handler action = do
    timer <- startTimer
    nextDelay <- liftIO $ T.newTVarIO Nothing
    fork $ 
        let setNextDelay = liftIO . atomically . T.writeTVar nextDelay . Just
            action'      = action >> timer >>= 
                            \passed -> setNextDelay (period - passed)
            handler' e   = handler e >>= setNextDelay
        in  action' `catch` handler'
 
    waitForRes nextDelay

  where
    continue = repeatForever period handler action

    waitForRes nextDelay = do
        wait $ for 10 ms
        res <- liftIO $ T.readTVarIO nextDelay
        case res of
            Nothing -> waitForRes nextDelay
            Just t  -> wait (for t mcs) >> continue

