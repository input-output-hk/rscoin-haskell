{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS_GHC -fno-cse #-}

-- | This module contains pure implementation of MonadTimed.

module RSCoin.Timed.Timed
       ( TimedT
       , runTimedT
       , evalTimedT
       , ThreadId
       ) where

import           Control.Exception.Base      (Exception)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (newTVarIO, readTVarIO, writeTVar)
import           Control.Exception           (SomeException)
import           Control.Exception.Base      (AsyncException (ThreadKilled))
import           Control.Lens                (makeLenses, to, use, view, (%=),
                                              (%~), (&), (.=), (<&>), (^.))
import           Control.Monad               (unless, void)
import           Control.Monad.Catch         (Handler (..), MonadCatch,
                                              MonadMask, MonadThrow, catch,
                                              catchAll, catches, mask, throwM,
                                              try, uninterruptibleMask)
import           Control.Monad.Cont          (ContT (..), runContT)
import           Control.Monad.Loops         (whileM_)
import           Control.Monad.Reader        (ReaderT (..), ask, runReaderT)
import           Control.Monad.State         (MonadState (get, put, state),
                                              StateT, evalStateT, modify)
import           Control.Monad.Trans         (MonadIO, MonadTrans, lift, liftIO)
import           Data.Function               (on)
import           Data.IORef                  (newIORef, readIORef, writeIORef)
import           Data.Maybe                  (fromJust)
import           Data.Ord                    (comparing)

import qualified Data.PQueue.Min             as PQ
import qualified Data.Set                    as S
import           Serokell.Util.Text          (formatSingle')

import           RSCoin.Core.Logging         (logWarning)
import           RSCoin.Timed.MonadTimed     (Microsecond, MonadTimed,
                                              MonadTimedError (MTTimeoutError),
                                              ThreadId (PureThreadId), for,
                                              fork, killThread, localTime, mcs,
                                             myThreadId, timeout, wait)

type Timestamp = Microsecond

-- | Private context for each pure thread
data ThreadCtx c = ThreadCtx
    { -- | Thread id
      _threadId :: ThreadId
      -- | Exception handlers stack. First is original handler,
      --   second is for continuation handler
    , _handlers :: [(Handler c (), Handler c ())]  -- ^ Exception handlers stack
    }

$(makeLenses ''ThreadCtx)

-- | Timestamped action
data Event m c = Event
    { _timestamp :: Timestamp
    , _action    :: m ()
    , _threadCtx :: ThreadCtx c
    }

$(makeLenses ''Event)

instance Eq (Event m c) where
    (==) = (==) `on` _timestamp

instance Ord (Event m c) where
    compare = comparing _timestamp

-- | Overall state for MonadTimed
data Scenario m c = Scenario
    { -- | set of sleeping threads
      _events         :: PQ.MinQueue (Event m c)
      -- | current virtual time
    , _curTime        :: Microsecond
      -- | set of declared threads.
      --   Implementation notes:
      --   when thread apperars, its id is added to set
      --   when thread is "killThread"ed, its id is removed
      --   when thread finishes it's execution, id remains in set
    , _aliveThreads   :: S.Set ThreadId
      -- | Number of created threads ever
    , _threadsCounter :: Integer
    }

$(makeLenses ''Scenario)

emptyScenario :: Scenario m c
emptyScenario =
    Scenario
    { _events = PQ.empty
    , _curTime = 0
    , _aliveThreads = S.empty
    , _threadsCounter = 0
    }

-- | Heart of TimedT monad
newtype Core m a = Core
    { getCore :: StateT (Scenario (TimedT m) (Core m)) m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow,
               MonadCatch, MonadMask)

instance MonadTrans Core where
    lift = Core . lift

instance MonadState s m => MonadState s (Core m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | Pure implementation of MonadTimed.
--   It stores an event queue, on "wait" continuation is passed to that queue
newtype TimedT m a = TimedT
    { unwrapTimedT :: ReaderT (ThreadCtx (Core m)) (ContT () (Core m)) a
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

newtype ContException = ContException SomeException
    deriving (Show)

instance Exception ContException

instance (MonadCatch m, MonadIO m) => MonadCatch (TimedT m) where
    catch m handler =
        TimedT $
        ReaderT $
        \r ->
            ContT $
            -- We can catch only from (m + its continuation).
            -- Thus, we should avoid handling exception from continuation.
            -- It's achieved by handling any exception and rethrowing it
            -- as ContException.
            -- Then, any catch handler should first check for ContException.
            -- If it's throw, rethrow exception inside ContException,
            -- otherwise handle original exception
            \c ->
                let safeCont x = getCore (c x) `catchAll` (throwM . ContException)
                    act = unwrapCore' r' $ m >>= wrapCore . safeCont
                    handler' e = unwrapCore' r $ handler e >>= wrapCore . getCore . c
                    contHandler (ContException e) = throwM e
                    r' = r & handlers %~ (:) (Handler (Core . handler'), Handler contHandler)
                in  Core $ act `catches` [Handler handler', Handler contHandler]

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

wrapCore :: Monad m => StateT (Scenario (TimedT m) (Core m)) m a -> TimedT m a
wrapCore = TimedT . lift . lift . Core

unwrapCore :: Monad m
           => ThreadCtx (Core m)
           -> (a -> Core m ())
           -> TimedT m a
           -> StateT (Scenario (TimedT m) (Core m)) m ()
unwrapCore r c = getCore
               . flip runContT c
               . flip runReaderT r
               . unwrapTimedT

unwrapCore' :: Monad m => ThreadCtx (Core m) -> TimedT m () -> StateT (Scenario (TimedT m) (Core m)) m ()
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
    mainThreadCtx >>= \ctx -> runInSandbox ctx timed
    -- event loop
    whileM_ notDone $ do
        -- take next event
        nextEv <- wrapCore $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        wrapCore $ curTime .= nextEv ^. timestamp


        let ctx = nextEv ^. threadCtx
            tid = ctx ^. threadId
        keepAlive <- wrapCore $ use $ aliveThreads . to (S.member tid)
        let -- die if time has come
            maybeDie = unless keepAlive $ throwM ThreadKilled
            act      = maybeDie >> runInSandbox ctx (nextEv ^. action)
            -- catch with handlers from handlers stack
            -- catch which is performed in instance MonadCatch is not enought
            -- cause on "wait" "catch" scope finishes, we need to catch
            -- again here
        wrapCore . getCore $ (Core (unwrapCore' ctx act)) `catchesSeq` (ctx ^. handlers)

  where
    notDone :: Monad m => TimedT m Bool
    notDone = wrapCore . use $ events . to (not . PQ.null)

    -- put empty continuation to an action (not our own!)
    runInSandbox r = wrapCore . unwrapCore' r

    mainThreadCtx = getNextThreadId <&>
        \tid ->
            ThreadCtx
            { _threadId = tid
            , _handlers = [( Handler threadKilledNotifier
                           , Handler $ \(ContException e) -> throwM e
                           )]
            }

    -- Apply all handlers from stack.
    -- If handled ContException, ignore second handler from that layer
    catchesSeq = foldl $ \act (h, hc) -> act `catches` [hc, h]

getNextThreadId :: Monad m => TimedT m ThreadId
getNextThreadId = do
    tid <- PureThreadId <$> wrapCore (use threadsCounter)
    wrapCore $ modify $ threadsCounter %~ (+1)
    wrapCore $ modify $ aliveThreads %~ S.insert tid
    return tid

-- | Just like runTimedT but makes it possible to get a result.
evalTimedT
    :: forall m a . (MonadIO m, MonadCatch m)
    => TimedT m a -> m a
evalTimedT timed = do
    ref <- liftIO $ newIORef Nothing
    runTimedT $ do
        m <- try timed
        liftIO . writeIORef ref . Just $ m
    res :: Either SomeException a <- fromJust <$> liftIO (readIORef ref)
    either throwM return res

threadKilledNotifier :: MonadIO m => SomeException -> m ()
threadKilledNotifier e =
    logWarning $ formatSingle' "Thread killed by exception: {}" $ show e

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadTimed (TimedT m) where
    localTime = wrapCore $ use curTime
    -- | Take note, created thread may be killed by timeout
    --   only when it calls "wait"
    fork _action = do
        -- just put new thread to an event queue
        _timestamp <- localTime
        tid <- getNextThreadId
        let _threadCtx =
                ThreadCtx
                { _threadId = tid
                , _handlers = [( Handler threadKilledNotifier
                               , Handler $ \(ContException e) -> throwM e
                               )]
                }
        wrapCore $ events %= PQ.insert Event { .. }
        return tid

    wait relativeToNow = do
        cur <- localTime
        ctx <- TimedT ask
        let event following =
                Event
                { _threadCtx = ctx
                , _timestamp = cur + relativeToNow cur
                , _action = wrapCore $ getCore $ following
                }
        -- grab our continuation, put it to event queue
        -- and finish execution
        TimedT $ lift $ ContT $
                  \c ->
                    Core $ events %= PQ.insert (event $ c ())

    myThreadId = TimedT $ view threadId

    killThread tid = wrapCore $ modify $ aliveThreads %~ S.delete tid

    -- TODO: we should probably implement this similar to
    -- http://haddock.stackage.org/lts-5.8/base-4.8.2.0/src/System-Timeout.html#timeout
    timeout t action' = do
        var <- liftIO $ newTVarIO Nothing
        -- fork worker
        wtid <- fork $ do
            res <- action'
            liftIO $ atomically $ writeTVar var $ Just res
        -- wait and gather results
        wait $ for t mcs
        killThread wtid
        res <- liftIO $ readTVarIO var
        maybe (throwM $ MTTimeoutError "Timeout exceeded") return res
