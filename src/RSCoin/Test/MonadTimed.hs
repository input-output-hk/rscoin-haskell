{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains time management monad and it's implementation for IO.

module RSCoin.Test.MonadTimed
    ( schedule, invoke
    , Timed
    , start, start_
    , minute , sec , ms , mcs
    , minute', sec', ms', mcs'
    , at
    , after
    , now
    , MicroSeconds
    , MonadTimed
    , RelativeToNow
    ) where

import           Control.Concurrent       (threadDelay, forkIO)
import           Control.Monad            (void)
import           Control.Monad.Trans      (liftIO, lift)
import           Control.Monad.Reader     (ReaderT, runReaderT, ask)
import           Control.Monad.State      (StateT, evalStateT, get)
import           Data.Time.Clock.POSIX    (getPOSIXTime)

-- Examples (wtf, they produce "not used" warnings, what should I do?)
sayHello :: IO ()
sayHello  =  start_ $ do
    invoke    now          $ liftIO $ putStrLn "Hello"
    invoke   (at    1 sec) $ liftIO $ putStrLn "1 second passed"
    liftIO                          $ putStrLn "Wow, it's 1 sec already"
    schedule (after 5 sec) $ liftIO $ putStrLn "5 more seconds passed, now 6"
    invoke   (after 2 sec) $ liftIO $ putStrLn "2 more seconds passed, now 8"

    schedule (at    2 sec 1 minute) $ liftIO $ putStrLn "Go to sleep"
 
nested :: IO ()
nested  =  start_ $ do
    schedule (at 2 sec) $ do
        liftIO $ print =<< getPOSIXTime
        schedule (after 1 sec) $ 
            liftIO $ print =<< getPOSIXTime
    invoke (at 5 sec) $ liftIO $ print =<< getPOSIXTime


type MicroSeconds  =  Int

-- | Defines some time point basing on current time point
type RelativeToNow  =  MicroSeconds -> MicroSeconds

-- | Allows time management. Time is specified in microseconds passed
--   from start point.
--   If scheduled time is already in past, action is executed immediatelly.
class Monad m => MonadTimed m where
    -- | Execute action somewhere in future
    schedule :: RelativeToNow -> m () -> m ()
    
    -- | Execute action and wait until it finishes
    invoke :: RelativeToNow -> m a -> m a

-- FIXME: is that ok to store rounded value?
type Timed a  =  ReaderT MicroSeconds IO a

instance MonadTimed (ReaderT MicroSeconds IO) where
    schedule timeMod action  =  do
        origin <- ask
        liftIO $ void $ forkIO $ do
            cur <- curTime
            threadDelay $ timeMod (cur - origin) - (cur - origin)
            void $ runReaderT action origin
 
    invoke timeMod  =  (>>) $ do
        origin <- ask
        liftIO $ do
            cur <- curTime
            threadDelay $ timeMod (cur - origin) - (cur - origin)

instance MonadTimed t => MonadTimed (StateT s t) where
    -- In most cases we can just fork the state
    schedule timeMod action  =         
        lift . schedule timeMod . evalStateT action =<< get

    -- Lower monad will not affect state
    invoke timeMod action  =  
        lift . invoke timeMod . evalStateT action =<< get
        

-- | Launches this timed action
start :: Timed a -> IO a
start  =  (curTime >>= ) . runReaderT

-- | Launches this timed action, ignoring the result
start_ ::  Timed a -> IO ()
start_  =  void . start

curTime :: IO MicroSeconds
curTime  =  ( * 1000000) . round <$> getPOSIXTime

-- TODO: make header
-- | Some usefull functions below

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
at :: TimeAcc t => t
at  =  at' 0

-- | Time point relative to current time
after :: TimeAcc t => t
after  =  after' 0

-- | Current time point. 
now  :: RelativeToNow
now  =  id

-- black magic 
class TimeAcc t where
    at'    :: MicroSeconds -> t
    after' :: MicroSeconds -> t

instance TimeAcc RelativeToNow where
    at'     =  id . const  
    after'  =  (+)

instance (a ~ b, TimeAcc t) => TimeAcc (a -> (b -> MicroSeconds) -> t) where
    at'    acc t f  =  at'    $ f t + acc
    after' acc t f  =  after' $ f t + acc



