{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Test.Timed
       ( TimedT
       , runTimedT
       ) where

import           Control.Exception      (SomeException)
import           Control.Exception.Base (AsyncException (ThreadKilled))
import           Control.Monad.Catch    (throwM)
import           Control.Lens           (makeLenses, to, use, (%=), (.=), (^.))
import           Control.Monad          (void, unless)
import           Control.Monad.Catch    (MonadCatch, MonadThrow, MonadMask, 
                                         catch, mask, uninterruptibleMask)
import           Control.Monad.Cont     (ContT (..), runContT)
import           Control.Monad.Loops    (whileM_)
import           Control.Monad.Reader   (ReaderT (..), ask, runReaderT)
import           Control.Monad.State    (MonadState (state, get, put), StateT,
                                         evalStateT)
import           Control.Monad.Trans    (MonadIO, MonadTrans, lift)
import           Data.Function          (on)
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)

import qualified Data.PQueue.Min        as PQ

import           RSCoin.Test.MonadTimed (MicroSeconds, MonadTimed, localTime,
                                         now, schedule, wait, workWhile,
                                         timeout)

type Timestamp = MicroSeconds

-- | Timestamped action
data Event m  =  Event
    { _timestamp :: Timestamp
    , _action    :: m ()
    , _condition :: m Bool
    }

$(makeLenses ''Event)

instance Eq (Event m) where
    (==)  =  (==) `on` _timestamp

instance Ord (Event m) where
    compare  =  comparing _timestamp

-- | State for MonadTimed
data Scenario m = Scenario
    { _events  :: PQ.MinQueue (Event m)
    , _curTime :: MicroSeconds
    }

$(makeLenses ''Scenario)

emptyScenario :: Scenario m
emptyScenario =
    Scenario
    { _events = PQ.empty
    , _curTime = 0
    }

-- | Pure implementation of MonadTimed.
--   It stores an event queue, on wait continuation is passed to that queue
newtype TimedT m a  =  TimedT
    { unwrapTimedT :: ReaderT (TimedT m Bool)
                     (ContT ()
                     (StateT (Scenario (TimedT m)) m)) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

-- | When stacking with other monads, take note of order of nesting.
--   For example, StateT above TimedT will clone it's state on fork, thus
--   all pure thread would have their own states. On the other hand,
--   StateT below TimedT would share it's state between all threads.
instance MonadTrans TimedT where
    lift = TimedT . lift . lift . lift

instance MonadState s m => MonadState s (TimedT m) where
    get = lift get
    put = lift . put
    state = lift . state

-- I don't understand why ConT monad is not an instance of MonadCatch
-- by default
instance MonadCatch m => MonadCatch (TimedT m) where
    catch (TimedT m) handler =
        TimedT $
        ReaderT $
        \r ->
             ContT $
             \c ->
                  catch (m' r c) (handler' r)
      where
        m' r c = runContT (runReaderT m r) c
        handler' r =
            flip runContT (return . const ()) .
            flip runReaderT r . unwrapTimedT . handler

instance MonadMask m => MonadMask (TimedT m) where
    mask a  =  TimedT $ ReaderT $ \r -> ContT $ \c -> 
        mask $ \u -> runContT (runReaderT (unwrapTimedT $ a $ q u) r) c
      where
        q u t  =  TimedT $ ReaderT $ \r -> ContT $ \c -> u $
            runContT (runReaderT (unwrapTimedT t) r) c
  
    uninterruptibleMask a  =  TimedT $ ReaderT $ \r -> ContT $ \c -> 
        uninterruptibleMask $ 
            \u -> runContT (runReaderT (unwrapTimedT $ a $ q u) r) c
      where
        q u t  =  TimedT $ ReaderT $ \r -> ContT $ \c -> u $
            runContT (runReaderT (unwrapTimedT t) r) c
        

launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT (TimedT t)  =  flip evalStateT emptyScenario
                         $  flip runContT   (void . return)
                         $  flip runReaderT (return True)
                         $  t

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
-- FIXME:  MonadCatch is not necessary here, we just should catch if it can throw
runTimedT :: (Monad m, MonadCatch m) => TimedT m () -> m ()
runTimedT timed  =  launchTimedT $ do
    schedule now timed `catch` handler
    whileM_ notDone $ do
        nextEv <- TimedT $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        TimedT $ curTime .= nextEv ^. timestamp

        let cond = nextEv ^. condition
        keepAlive <- cond
        -- We can't just invoke (nextEv ^. action) here, because it can put
        -- further execution to event queue or even throw it away.
        -- We want to successfully finish this action and go to next iteration
        -- rather than lose execution control
        let TimedT act = nextEv ^. action
            act'       = TimedT . lift . lift
                       $ runContT (runReaderT act cond) return
            maybeDie   = unless keepAlive $ throwM ThreadKilled
        (maybeDie >> act') `catch` handler
  where
    notDone :: Monad m => TimedT m Bool
    notDone = TimedT . use $ events . to (not . PQ.null)

    handler :: Monad m => SomeException -> TimedT m ()
    handler _ = return ()   -- TODO: log here

instance MonadThrow m => MonadTimed (TimedT m) where
    localTime = TimedT $ use curTime

    -- | Take note, created thread may be killed by timeout
    --   only when it calls "wait"
    workWhile _condition _action = do
        _timestamp <- localTime
        TimedT $ events %= PQ.insert Event { .. }

    wait relativeToNow = do
        cur <- localTime
        cond <- TimedT $ ask
        let event following =
                Event
                { _condition = cond
                , _timestamp = cur + relativeToNow cur
                , _action = TimedT $ lift . lift $ following ()
                }
        TimedT $ lift $ ContT $
            \following ->
                 events %= PQ.insert (event following)
    -- FIXME: !
    timeout _ = id
