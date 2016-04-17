{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Test.Timed
       ( TimedT
       , startTimedT
       , runTimedT
       ) where

import           Control.Applicative         ((<$>))
import           Control.Monad               (void)
import           Control.Monad.Random        (RandT, evalRandT)
import           Control.Monad.State
import           Control.Monad.Trans         (lift, liftIO)
import           Control.Monad.Trans.Maybe   (MaybeT(..), runMaybeT)
import           Control.Monad.Cont          (ContT(..), runContT)
import           Control.Monad.Loops         (whileM)
import           Data.Default                (Default, def)
import           Control.Lens                ((+~), (&), (%~), (?~), (^.), (%=)
                                             , (.=), makeLenses, at, view, _3
                                             , to, use)
import           Data.Ord                    (comparing) 
import           Data.Maybe                  (fromJust)
import           Data.Function               (on)
import           Data.MessagePack.Object     (Object(..), MessagePack, toObject, fromObject)

import qualified Data.PQueue.Min             as PQ
 
import           RSCoin.Test.MonadTimed
import           RSCoin.Test.MonadRpc

type Timestamp = MicroSeconds

-- timestamped action
data Event m  =  Event 
    { _timestamp :: Timestamp
    , _action    :: m ()
    } 
$(makeLenses ''Event)
    
instance Eq (Event m) where
    (==)  =  (==) `on` _timestamp 

instance Ord (Event m) where
    compare  =  comparing _timestamp 

-- state for MonadTimed
data Scenario m = Scenario 
    { _events    :: PQ.MinQueue (Event m)
    , _curTime   :: MicroSeconds
    }
$(makeLenses ''Scenario)

-- | Pure implementation of MonadTimed. 
--   It stores an event queue, on wait continuation is passed to that queue
newtype TimedT m a = TimedT (ContT () (StateT (Scenario (TimedT m)) m) a)
    deriving (Functor, Applicative, Monad)

-- | When stacking with other monads, take note of order of nesting.
--   For example, StateT above TimedT will clone it's state on fork, thus
--   all pure thread would have their own states. On the other hand, 
--   StateT below TimedT would share it's state between all threads.
instance MonadTrans TimedT where
    lift  =  TimedT . lift . lift

instance MonadIO m => MonadIO (TimedT m) where
    liftIO  =  TimedT . liftIO

-- | Unwraps TimedT
runTimedT :: Monad m => TimedT m () -> m ()
runTimedT (TimedT t) = evalStateT (runContT t (void . return)) scenario
  where
    scenario = Scenario 
        { _events    = PQ.empty
        , _curTime   = 0
        }

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
startTimedT :: Monad m => TimedT m () -> m ()
startTimedT timed  =  runTimedT . (timed >>) . void . whileM notDone $ do
    nextEv <- TimedT . lift $ do
        (ev, evs') <- fromJust . PQ.minView <$> use events
        events .= evs'
        return ev
    TimedT $ lift $ curTime .= _timestamp nextEv
    nextEv ^. action
  where
    notDone :: Monad m => TimedT m Bool
    notDone  =  TimedT . lift . use $ events . to PQ.null

instance Monad m => MonadTimed (TimedT m) where
    localTime = TimedT . lift $ gets _curTime
    
    fork _action  =  do
        _timestamp  <- localTime
        TimedT $ lift $ events %= PQ.insert Event{..}

    wait timeMod  =  do
        _timestamp <- timeMod <$> localTime
        TimedT $ ContT $ \following ->
            let _action = TimedT $ lift $ following ()
            in  events %= PQ.insert Event{..} 

