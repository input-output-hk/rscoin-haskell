{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Timed.Timed
       ( TimedT
       , runTimedT
       , evalTimedT
       ) where

import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVar, readTVar,
                                              writeTVar)
import           Control.Exception           (SomeException)
import           Control.Exception.Base      (AsyncException (ThreadKilled))
import           Control.Lens                (makeLenses, to, use, (%=), (%~),
                                              (&), (.=), (^.))
import           Control.Monad               (unless, void, when)
import           Control.Monad.Catch         (Handler (..), MonadCatch,
                                              MonadMask, MonadThrow, catch,
                                              catchAll, catches, mask, throwM,
                                              uninterruptibleMask)
import           Control.Monad.Cont          (ContT (..), runContT)
import           Control.Monad.Loops         (whileM_)
import           Control.Monad.Reader        (ReaderT (..), ask, runReaderT)
import           Control.Monad.State         (MonadState (get, put, state),
                                              StateT, evalStateT)
import           Control.Monad.Trans         (liftIO)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift)
import           Data.Function               (on)
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Maybe                  (catMaybes, fromJust, fromMaybe,
                                              isNothing)
import           Data.Ord                    (comparing)
import           Safe                        (headMay)

import qualified Data.PQueue.Min             as PQ
import           Serokell.Util.Text          (formatSingle')

import           RSCoin.Core.Logging         (logWarning)
import           RSCoin.Timed.MonadTimed     (Microsecond, MonadTimed,
                                              MonadTimedError (MTTimeoutError),
                                              after, localTime, mcs, timeout,
                                              wait, workWhile)


type Timestamp = Microsecond

-- | Private context for each pure thread
data ThreadCtx m = ThreadCtx
    { _condition :: m Bool          -- ^ Whether thread should remain alive
    , _handlers  :: [Handler m ()]  -- ^ Exception handlers stack
    }

$(makeLenses ''ThreadCtx)

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
    , _curTime :: Microsecond
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
--   It stores an event queue, on "wait" continuation is passed to that queue
newtype TimedT m a = TimedT
    { unwrapTimedT :: ReaderT (ThreadCtx (TimedT m)) (ContT () (Core m)) a
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

instance (MonadCatch m, MonadIO m) => MonadCatch (TimedT m) where
    catch m handler =
        TimedT $
        ReaderT $
        \r ->
            ContT $
            \c -> do
            -- dirty hack
            -- catch from (m + it's continuation)
            -- if continuation throws, store exception into variable
            -- and then rethrow
            -- (all that because TimedT can't return a sensible value
            --  when unwrapped :< )
                contExcept <- liftIO $ newIORef Nothing
                let remE = liftIO . writeIORef contExcept . Just
                    act = m >>= \x -> wrapCore $ c x `catchAll` remE
                    handler' e = handler e >>= wrapCore . c
                    r' = r & handlers %~ (Handler handler' : )
                unwrapCore' r' act `catch` (unwrapCore' r . handler')
                e <- liftIO $ readIORef contExcept
                maybe (return ()) throwM e

-- Posibly incorrect instance
instance (MonadIO m, MonadMask m) => MonadMask (TimedT m) where
    mask a = TimedT $ ReaderT $ \r -> ContT $ \c ->
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

wrapCore :: Monad m => Core m a -> TimedT m a
wrapCore = TimedT . lift . lift

unwrapCore :: Monad m
           => ThreadCtx (TimedT m)
           -> (a -> Core m ())
           -> TimedT m a
           -> Core m ()
unwrapCore r c = flip runContT c
               . flip runReaderT r
               . unwrapTimedT

unwrapCore' :: Monad m => ThreadCtx (TimedT m) -> TimedT m () -> Core m ()
unwrapCore' r = unwrapCore r return

launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT t = flip evalStateT emptyScenario
               $ unwrapCore vacuumCtx (void . return) t
  where
    vacuumCtx = error "Access to thread context from nowhere"

-- | Starts timed evaluation. Finishes when no more scheduled actions remain.
runTimedT :: (MonadIO m, MonadCatch m) => TimedT m () -> m ()
runTimedT timed = launchTimedT $ do
    -- execute first action (main thread)
    runInSandbox mainThreadCtx timed
    -- event loop
    whileM_ notDone $ do
        -- take next event
        nextEv <- TimedT $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        TimedT $ curTime .= nextEv ^. timestamp

        let ctx = nextEv ^. threadCtx
        keepAlive <- ctx ^. condition
        let -- die if time has come
            maybeDie = unless keepAlive $ throwM ThreadKilled
            act      = maybeDie >> runInSandbox ctx (nextEv ^. action)
            -- catch with handlers from handlers stack
        act `catches` (ctx ^. handlers)
  where
    notDone :: Monad m => TimedT m Bool
    notDone = TimedT . use $ events . to (not . PQ.null)

    -- put empty continuation to an action (not our own!)
    runInSandbox r = wrapCore . unwrapCore' r

    mainThreadCtx =
        ThreadCtx
        { _condition = return True
        , _handlers  = [Handler threadKilledNotifier]
        }

-- | Just like runTimedT but makes it possible to get a result.
evalTimedT
    :: (MonadIO m, MonadCatch m)
    => TimedT m a -> m a
evalTimedT timed = do
    ref <- liftIO $ newIORef Nothing
    runTimedT $ liftIO . writeIORef ref . Just =<< timed
    fromJust <$> liftIO (readIORef ref)

threadKilledNotifier :: MonadIO m => SomeException -> m ()
threadKilledNotifier e =
    logWarning $ formatSingle' "Thread killed by exception: {}" $ show $ e
{-# NOINLINE threadKilledNotifier #-}

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadTimed (TimedT m) where
    localTime = TimedT $ use curTime

    -- | Take note, created thread may be killed by timeout
    --   only when it calls "wait"
    workWhile cond _action = do
        -- just put new thread to an event queue
        _timestamp <- localTime
        let _threadCtx =
                ThreadCtx
                { _condition = cond
                , _handlers  = [Handler threadKilledNotifier]
                }
        TimedT $ events %= PQ.insert Event { .. }

    wait relativeToNow = do
        cur <- localTime
        ctx <- TimedT ask
        let event following =
                Event
                { _threadCtx = ctx
                , _timestamp = cur + relativeToNow cur
                , _action = wrapCore following
                }
        -- grab our continuation, put it to event queue
        -- and finish execution
        TimedT $ lift $ ContT $
                  \c -> do
                    events %= PQ.insert (event $ c ())
    timeout t action' = do
        var <- liftIO $ atomically $ newTVar Nothing
        timestamp' <- localTime
        workWhile (untilTime $ t + timestamp') $ do
            res <- action'
            liftIO $ atomically $ writeTVar var $
                Just $ return res
        -- NOTE: this is checking TVar in order to find one result (either error, or some value)
        -- every microsecond. Testing performanse could be slightly worse with this. Proper way would
        -- be to implement ThreadId and killThread in MonadTimed, then this would be avoided.
        --
        -- Depending on MonadTimed (TimedT) internals, actions might take 1 microsecond longer to finish.
        -- It takes at least one microsecond to collect and return the results (next line).
        -- Doing `timeout 10 (wait $ for 5 mcs)` might return after 6 mcs (in this very exaple it will return after 100 mcs, depending on sleepStep)
        k <- fromMaybe (throwM $ MTTimeoutError "Timeout exceeeded") . headMay . catMaybes
             <$> (sequence $ Prelude.replicate ((+1) . ceiling $ fromIntegral t / fromIntegral sleepStep) $ getMaybeValue var)
        lift k
      where
        sleepStep = 100 :: Microsecond
        getMaybeValue :: (MonadIO m, MonadThrow m, MonadCatch m) => TVar (Maybe (m a)) -> TimedT m (Maybe (m a))
        getMaybeValue var = do
            res <- liftIO $ atomically $ readTVar var
            when (isNothing res) $
                -- NOTE: @martoon said 100 mcs is sleep time resolution in threads.
                -- Sleeping only 1 mcs might hit the preformance wall.
                wait $ after sleepStep mcs
            return res
        untilTime :: (MonadIO m, MonadThrow m, MonadCatch m) => Microsecond -> TimedT m Bool
        untilTime time = (time >) <$> localTime
