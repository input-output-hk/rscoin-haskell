{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Test.Timed
       ( TimedT
       , runTimedT
       ) where

import           Control.Monad               (void)
import           Control.Monad.Catch         (MonadThrow, throwM
                                             , MonadCatch, catch, Exception)
import           Control.Exception           (SomeException)
import           Control.Monad.State         (StateT, evalStateT, gets)
import           Control.Monad.Trans         (lift, liftIO, MonadTrans, MonadIO)
import           Control.Monad.Cont          (ContT(..), runContT)
import           Control.Monad.Loops         (whileM)
import           Control.Lens                ((%=), (.=), (^.), to, use
                                             , makeLenses, makeLensesFor)
import           Data.Ord                    (comparing) 
import           Data.Maybe                  (fromJust)
import           Data.Function               (on)

import qualified Data.PQueue.Min             as PQ
 
import           RSCoin.Test.MonadTimed      (MonadTimed, MicroSeconds
                                             , fork, wait, localTime, now
                                             , schedule)

type Timestamp = MicroSeconds

-- | Timestamped action
data Event m  =  Event 
    { _timestamp :: Timestamp
    , _action    :: m ()
    } 
$(makeLensesFor [("_action", "action")] ''Event)
    
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
    { unwrapTimedT :: ContT () (StateT (Scenario (TimedT m)) m) a
    } deriving (Functor, Applicative, Monad)

-- | When stacking with other monads, take note of order of nesting.
--   For example, StateT above TimedT will clone it's state on fork, thus
--   all pure thread would have their own states. On the other hand, 
--   StateT below TimedT would share it's state between all threads.
instance MonadTrans TimedT where
    lift  =  TimedT . lift . lift

instance MonadIO m => MonadIO (TimedT m) where
    liftIO  =  TimedT . liftIO

instance MonadThrow m => MonadThrow (TimedT m) where
    throwM  =  TimedT . throwM
 
-- I don't understand why ConT monad is not an instance of MonadCatch
-- by default   
instance MonadCatch m => MonadCatch (TimedT m) where
    catch (TimedT (ContT m)) handler  =  TimedT $ ContT $ \c -> 
        catch (m c) handler'
      where
        handler'  =  flip runContT (return . const ()) . unwrapTimedT . handler


launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT (TimedT t) = evalStateT (runContT t (void . return)) Scenario{..}
  where
    _events  = PQ.empty
    _curTime = 0    

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
-- FIXME:  MonadCatch is not necessary here, we just should catch if it can throw
runTimedT :: (Monad m, MonadCatch m) => TimedT m () -> m ()
runTimedT timed  =  launchTimedT $ do
    schedule now timed `catch` handler 
    void . whileM notDone $ do
        nextEv <- TimedT . lift $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        TimedT $ lift $ curTime .= _timestamp nextEv
 
        -- We can't just invoke (nextEv ^. action) here, because it can put
        -- further execution to event queue or even throw it away. 
        -- We want to successfully finish this action and go to next iteration 
        -- rather than loose execution control
        let TimedT act = nextEv ^. action
        (TimedT $ lift $ runContT act return) `catch` handler
  where
    notDone :: Monad m => TimedT m Bool
    notDone  =  TimedT . lift . use $ events . to (not . PQ.null)
    
    handler :: Monad m => SomeException -> TimedT m ()
    handler _  =  return ()   -- TODO: log here

instance Monad m => MonadTimed (TimedT m) where
    localTime = TimedT . lift $ gets _curTime
    
    fork _action  =  do
        _timestamp <- localTime
        TimedT $ lift $ events %= PQ.insert Event{..}

    wait relativeToNow  =  do 
        cur            <- localTime
        let _timestamp =  cur + relativeToNow cur 
        TimedT $ ContT $ \following ->
            let _action = TimedT $ lift $ following ()
            in  events %= PQ.insert Event{..} 



