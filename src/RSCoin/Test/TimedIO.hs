{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}

-- | IO-based implementation of MonadTimed type class.

module RSCoin.Test.TimedIO
       ( TimedIO
       , runTimedIO
       , runTimedIO_
       ) where

import           Control.Concurrent          (forkIO, killThread, threadDelay)
import           Control.Monad               (void)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadCatch, MonadThrow)
import           Control.Monad.Loops         (whileM)
import           Control.Monad.Reader        (ReaderT (..), ask, runReaderT)
import           Control.Monad.Trans         (MonadIO, lift, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl, StM,
                                              liftBaseWith, restoreM)
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Time.Clock.POSIX       (getPOSIXTime)

import           RSCoin.Test.MonadTimed      (MicroSeconds, MonadTimed (..))

newtype TimedIO a = TimedIO
    { getTimedIO :: ReaderT MicroSeconds IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
               , MonadBase IO)

instance MonadBaseControl IO TimedIO where
    type StM TimedIO a = a

    liftBaseWith f = TimedIO $ liftBaseWith $ \g -> f $ g . getTimedIO

    restoreM = TimedIO . restoreM

instance MonadTimed TimedIO where
    localTime = TimedIO $ (-) <$> lift curTime <*> ask

    fork (TimedIO a) = TimedIO $ lift . void . forkIO . runReaderT a =<< ask

    wait relativeToNow = do
        cur <- localTime
        liftIO $ threadDelay $ relativeToNow cur

    workWhile (TimedIO p) (TimedIO action) = TimedIO $ do
        env     <- ask
        working <- lift $ newIORef True

        tid <- lift . forkIO $ do
            runReaderT action env
            writeIORef working False

        lift . void . forkIO $ do
            _ <- whileM ((&&) <$> runReaderT p env <*> readIORef working) $
                threadDelay 100000
            killThread tid

-- | Launches this timed action
runTimedIO :: TimedIO a -> IO a
runTimedIO = (curTime >>= ) . runReaderT . getTimedIO

-- | Launches this timed action, ignoring the result
runTimedIO_ ::  TimedIO a -> IO ()
runTimedIO_ = void . runTimedIO

curTime :: IO MicroSeconds
curTime = round . ( * 1000000) <$> getPOSIXTime
