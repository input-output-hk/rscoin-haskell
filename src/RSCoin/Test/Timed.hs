{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Test.Timed
       ( TimedT
       , runTimedT
       ) where

import           Control.Exception      (SomeException)
import           Control.Lens           (makeLenses, to, use, (%=), (.=))
import           Control.Monad          (void, when)
import           Control.Monad.Catch    (MonadCatch, MonadThrow, catch)
import           Control.Monad.Cont     (ContT (..), runContT)
import           Control.Monad.Loops    (whileM)
import           Control.Monad.Reader   (MonadReader, ReaderT (..), ask,
                                         runReaderT)
import           Control.Monad.State    (MonadState, StateT, evalStateT)
import           Control.Monad.Trans    (MonadIO, MonadTrans, lift)
import           Data.Function          (on)
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)

import qualified Data.PQueue.Min        as PQ

import           RSCoin.Test.MonadTimed (MicroSeconds, MonadTimed, localTime,
                                         now, schedule, wait, workWhile)

type Timestamp = MicroSeconds

-- | Timestamped action
data Event m  =  Event
    { _timestamp :: Timestamp
    , _action    :: m ()
    , _condition :: m Bool
    }

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

-- | Pure implementation of MonadTimed.
--   It stores an event queue, on wait continuation is passed to that queue
newtype TimedT m a  =  TimedT
    { unwrapTimedT :: ReaderT (TimedT m Bool)
                     (ContT ()
                     (StateT (Scenario (TimedT m)) m)) a
    } deriving (Functor, Applicative, Monad, MonadReader (TimedT m Bool), MonadState (Scenario (TimedT m)), MonadIO, MonadThrow)

-- | When stacking with other monads, take note of order of nesting.
--   For example, StateT above TimedT will clone it's state on fork, thus
--   all pure thread would have their own states. On the other hand,
--   StateT below TimedT would share it's state between all threads.
instance MonadTrans TimedT where
    lift = TimedT . lift . lift . lift

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

launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT (TimedT t)  =  flip evalStateT Scenario{..}
                         $  flip runContT   (void . return)
                         $  flip runReaderT (return True)
                         $  t
  where
    _events  = PQ.empty
    _curTime = 0

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
-- FIXME:  MonadCatch is not necessary here, we just should catch if it can throw
runTimedT :: (Monad m, MonadCatch m) => TimedT m () -> m ()
runTimedT timed  =  launchTimedT $ do
    schedule now timed `catch` handler
    void . whileM notDone $ do
        nextEv <- do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        TimedT $ lift $ lift $ curTime .= _timestamp nextEv

        let cond = _condition nextEv
        ok <- cond
        -- We can't just invoke (nextEv ^. action) here, because it can put
        -- further execution to event queue or even throw it away.
        -- We want to successfully finish this action and go to next iteration
        -- rather than loose execution control
        when ok $ let TimedT act = _action nextEv
                      act'       = TimedT $ lift $ lift
                                 $ runContT (runReaderT act cond) return
                  in  act' `catch` handler
  where
    notDone :: Monad m => TimedT m Bool
    notDone  =  use $ events . to (not . PQ.null)

    handler :: Monad m => SomeException -> TimedT m ()
    handler _  =  return ()   -- TODO: log here

instance Monad m => MonadTimed (TimedT m) where
    localTime = use curTime

    -- | Take note, created thread may be killed by timeout
    --   only when it calls "wait"
    workWhile _condition _action = do
        _timestamp <- localTime
        events %= PQ.insert Event { .. }

    wait relativeToNow = do
        cur <- localTime
        cond <- ask
        let event following =
                Event
                { _condition = cond
                , _timestamp = cur + relativeToNow cur
                , _action = TimedT $ lift . lift $ following ()
                }
        TimedT $ lift $ ContT $
            \following ->
                 events %= PQ.insert (event following)
