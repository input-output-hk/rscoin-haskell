{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains time management monad and it's implementation for IO.
module RSCoin.Test.MonadTimed
    ( fork, wait, localTime, schedule, invoke
    , TimedIO
    , runTimedIO, runTimedIO_
    , minute , sec , ms , mcs
    , minute', sec', ms', mcs'
    , at, after, for, till, now
    , MicroSeconds
    , MonadTimed
    , RelativeToNow
    ) where

import           Control.Concurrent          (threadDelay, forkIO)
import           Control.Monad               (void)
import           Control.Monad.Base          (MonadBase)
import           Control.Monad.Catch         (MonadThrow, MonadCatch)
import           Control.Monad.Trans         (liftIO, lift, MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith, restoreM, StM)
import           Control.Monad.Reader        (ReaderT, runReaderT, ask)
import           Data.Time.Clock.POSIX       (getPOSIXTime)

type MicroSeconds  =  Int

-- | Defines some time point basing on current time point
type RelativeToNow  =  MicroSeconds -> MicroSeconds

-- | Allows time management. Time is specified in microseconds passed
--   from start point (origin).
class Monad m => MonadTimed m where
    -- | Acquires time relative to origin point
    localTime :: m MicroSeconds

    -- | Creates another thread of execution, with same point of origin
    fork :: m () -> m ()

    -- | Waits till specified relative time
    wait :: RelativeToNow -> m ()

-- | Executes an action somewhere in future
schedule :: MonadTimed m => RelativeToNow -> m () -> m ()
schedule time action  =  fork $ wait time >> action
 
-- | Executes an action at specified time in current thread
invoke :: MonadTimed m => RelativeToNow -> m a -> m a
invoke time action  =  wait time >> action
   
-- FIXME: is that ok to store time in microseconds?
newtype TimedIO a  =  TimedIO 
    { getTimedIO :: ReaderT MicroSeconds IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch
               , MonadBase IO)

instance MonadBaseControl IO TimedIO where
    type StM TimedIO a = a
    
    liftBaseWith f = undefined
    restoreM = undefined

    

instance MonadTimed TimedIO where
    localTime  =  TimedIO $ (-) <$> lift curTime <*> ask

    fork (TimedIO a)  =  TimedIO $ lift . void . forkIO . runReaderT a =<< ask

    wait relativeToNow  =  do
        cur <- localTime
        liftIO $ threadDelay $ relativeToNow cur 

instance MonadTimed m => MonadTimed (ReaderT r m) where
    localTime  =  lift localTime 
    
    fork m  =  lift . fork . runReaderT m =<< ask

    wait  =  lift . wait

-- | Launches this timed action
runTimedIO :: TimedIO a -> IO a
runTimedIO  =  (curTime >>= ) . runReaderT . getTimedIO

-- | Launches this timed action, ignoring the result
runTimedIO_ ::  TimedIO a -> IO ()
runTimedIO_  =  void . runTimedIO

curTime :: IO MicroSeconds
curTime  =  ( * 1000000) . round <$> getPOSIXTime

-- * Some usefull functions below

-- | Defines measure for time periods
mcs, ms, sec, minute :: Int -> MicroSeconds
mcs     =  id
ms      =  (*) 1000
sec     =  (*) 1000000
minute  =  (*) 60000000

mcs', ms', sec', minute' :: Double -> MicroSeconds
mcs'     =  round
ms'      =  round . (*) 1000
sec'     =  round . (*) 1000000
minute'  =  round . (*) 60000000

-- | Time point by given absolute time (still relative to origin)
at, till :: TimeAcc t => t
at   =  at' 0
till =  at' 0

-- | Time point relative to current time
after, for :: TimeAcc t => t
after  =  after' 0
for    =  after' 0

-- | Current time point 
now  :: RelativeToNow
now  =  id

-- black magic 
class TimeAcc t where
    at'    :: MicroSeconds -> t
    after' :: MicroSeconds -> t

instance TimeAcc RelativeToNow where
    at'     =  (-)  
    after'  =  const

instance (a ~ b, TimeAcc t) => TimeAcc (a -> (b -> MicroSeconds) -> t) where
    at'    acc t f  =  at'    $ f t + acc
    after' acc t f  =  after' $ f t + acc



