{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Test.Timed
       ( TimedT
       , runTimedT
       ) where

import           Control.Exception      (SomeException)
import           Control.Exception.Base (AsyncException (ThreadKilled))
import           Control.Lens           (makeLenses, to, use, (%=), (.=), (^.),
                                         view, (|>), (&), (%~))
import           Control.Monad          (void, unless)
import           Control.Monad.Catch    (MonadCatch, MonadThrow, MonadMask, 
                                         catch, mask, uninterruptibleMask,
                                         catches, Handler(..), throwM, catchAll)
import           Control.Monad.Cont     (ContT (..), runContT)
import           Control.Monad.Loops    (whileM_)
import           Control.Monad.Reader   (ReaderT (..), ask, runReaderT)
import           Control.Monad.State    (MonadState (state, get, put), StateT,
                                         evalStateT)

import           Control.Monad.Trans    (MonadIO, MonadTrans, lift)
import           Data.Function          (on)
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)
import           Data.Text              as T
import           Data.IORef             (newIORef, readIORef, writeIORef)
import           System.IO.Unsafe       (unsafePerformIO)

import qualified Data.PQueue.Min        as PQ
import           Serokell.Util.Text     (formatSingle')

import           RSCoin.Test.MonadTimed (MicroSeconds, MonadTimed, localTime,
                                         now, schedule, wait, workWhile,
                                         timeout)
import           RSCoin.Core.Logging    (logWarning)
-- import           RSCoin.Test.ContA      (ContAT, contAT, runContAT)


import Control.Monad.Trans (liftIO)
import Debug.Trace (trace, traceIO, traceM)

import Prelude 
import qualified Prelude as P

type Timestamp = MicroSeconds

-- | Private context for each pure thread
data ThreadCtx m = ThreadCtx
    { _condition :: m Bool          -- ^ Whether thread should remain alive
    , _handlers  :: [Handler m ()]  -- ^ Exception handlers stack
    }

$(makeLenses ''ThreadCtx)

initThreadCtx :: Monad m => ThreadCtx m
initThreadCtx = 
    ThreadCtx
    { _condition = return True
    , _handlers  = []
    }

-- | Timestamped action
data Event m = Event
    { _timestamp :: Timestamp
    , _action    :: m ()
    , _threadCtx :: ThreadCtx m
    }

$(makeLenses ''Event)

instance Eq (Event m) where
    (==) = (==) `on` _timestamp

instance Ord (Event m) where
    compare = comparing _timestamp

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

-- | Heart of TimedT monad
type Core m = StateT (Scenario (TimedT m)) m

-- | Pure implementation of MonadTimed.
--   It stores an event queue, on wait continuation is passed to that queue
newtype TimedT m a = TimedT
    { unwrapTimedT :: ReaderT (ThreadCtx (TimedT m))
                     (ContT () (Core m)) a
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

instance MonadCatch m => MonadCatch (ContT r m) where
    catch (ContT m) h = ContT $ \c -> catch (m c) (\e -> (runContT $ h e) c)

instance (MonadCatch m, MonadIO m) => MonadCatch (TimedT m) where
    catch m handler =
        TimedT $
        ReaderT $
        \r ->
            ContT $
            \c -> do
                contExcept <- liftIO $ newIORef Nothing
                let act = m >>= \x -> TimedT $ lift $ lift $ c x `catchAll` (liftIO . writeIORef contExcept . Just)
                    handler' e = do
                        x <- handler e
                        TimedT $ lift $ lift $ c x 
                    r' = r & handlers %~ ( ++ [Handler handler'])
                (runTwoLayers r' return act) `catch` (runTwoLayers r return . handler')
                e <- liftIO (readIORef contExcept)
                maybe (return ()) throwM e 
--                catch (m' r return) (handler' r) >>= c
      where
        runTwoLayers r c m = runContT (runReaderT (unwrapTimedT m) r) c
        m' r c = runContT (runReaderT (unwrapTimedT m) r) c
        modify c = return 

--        handler' r =
--            flip runContT (return . const ()) .
--            flip runReaderT r . unwrapTimedT . handler

 
-- Posibly incorrect instance
instance (MonadIO m, MonadMask m) => MonadMask (TimedT m) where
{-    mask a = TimedT $ ReaderT $ \r -> ContT $ \c -> 
        mask $ \u -> runContT (runReaderT (unwrapTimedT $ a $ q u) r) c
      where
        q u t = TimedT $ ReaderT $ \r -> ContT $ \c -> u $
            runContT (runReaderT (unwrapTimedT t) r) c
  
    uninterruptibleMask a = TimedT $ ReaderT $ \r -> ContT $ \c -> 
        uninterruptibleMask $ 
            \u -> runContT (runReaderT (unwrapTimedT $ a $ q u) r) c
      where
        q u t = TimedT $ ReaderT $ \r -> ContT $ \c -> u $
            runContT (runReaderT (unwrapTimedT t) r) c
-}      

launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT (TimedT t) = flip evalStateT emptyScenario
                        $ flip runContT   (void . return)
                        $ flip runReaderT initThreadCtx
                        $ t

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
runTimedT :: (MonadIO m, MonadCatch m) => TimedT m () -> m ()
runTimedT timed = launchTimedT $ do
    schedule now timed 
    whileM_ notDone $ do
        nextEv <- TimedT $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        TimedT $ curTime .= nextEv ^. timestamp

        let ctx = nextEv ^. threadCtx 
        keepAlive <- ctx ^. condition
        let TimedT act = nextEv ^. action
            -- put empty continuation to an action (not our own!)
            act'     = runContT (runReaderT act ctx) return
            act_     = flip runContT return $ flip runReaderT ctx $ unwrapTimedT $
                        (TimedT $ lift $ lift $ act') `catches` (ctx ^. handlers)
            act''    = (maybeDie >> act_) `catch` handler
            act'''   = TimedT . lift . lift $ act''
            maybeDie = unless keepAlive $ throwM ThreadKilled
        act'''
  where
    notDone :: Monad m => TimedT m Bool
    notDone = TimedT . use $ events . to (not . PQ.null)

    handler :: Monad m => SomeException -> m ()
    handler e = let text = formatSingle' "Thread killed by exception: {}" $ 
                           T.pack . show $ e
                in  return $! unsafePerformIO $ logWarning text
    {-# NOINLINE handler #-}

instance MonadThrow m => MonadTimed (TimedT m) where
    localTime = TimedT $ use curTime

    -- | Take note, created thread may be killed by timeout
    --   only when it calls "wait"
    workWhile cond _action = do
        _timestamp <- localTime
        let _threadCtx = 
                ThreadCtx
                { _condition = cond
                , _handlers  = []
                }
        TimedT $ events %= PQ.insert Event { .. }

    wait relativeToNow = do
        cur <- localTime
        ctx <- TimedT ask
        let event following =
                Event
                { _threadCtx = ctx
                , _timestamp = cur + relativeToNow cur
                , _action = TimedT $ lift . lift $ following
                }
        TimedT $ lift $ ContT $ 
                  \c -> do
                    events %= PQ.insert (event $ c ())
    -- FIXME: !
    timeout _ = id
