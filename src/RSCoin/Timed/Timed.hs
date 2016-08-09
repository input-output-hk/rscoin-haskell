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

import           Control.Exception.Base   (AsyncException (ThreadKilled),
                                           Exception (fromException),
                                           SomeException (..))

import           Control.Lens             (makeLenses, to, use, view, (%=),
                                           (%~), (&), (+=), (.=), (<&>), (^.))
import           Control.Monad            (unless, void)
import           Control.Monad.Catch      (Handler (..), MonadCatch, MonadMask,
                                           MonadThrow, catch, catchAll, catches,
                                           mask, throwM, try,
                                           uninterruptibleMask)
import           Control.Monad.Cont       (ContT (..), runContT)
import           Control.Monad.Loops      (whileM_)
import           Control.Monad.Reader     (ReaderT (..), ask, runReaderT)
import           Control.Monad.State      (MonadState (get, put, state), StateT,
                                           evalStateT)
import           Control.Monad.Trans      (MonadIO, MonadTrans, lift, liftIO)
import           Data.Function            (on)
import           Data.IORef               (newIORef, readIORef, writeIORef)
import           Data.List                (foldl')
import           Data.Maybe               (fromJust)
import           Data.Ord                 (comparing)
import           Formatting               (sformat, shown, (%))

import qualified Data.PQueue.Min          as PQ
import qualified Data.Set                 as S

import           RSCoin.Core.Logging      (logDebug, logWarning)
import           RSCoin.Core.NamedLogging (WithNamedLogger (..))
import           RSCoin.Timed.MonadTimed  (Microsecond, Millisecond, MonadTimed,
                                           MonadTimedError (MTTimeoutError),
                                           ThreadId (PureThreadId), for, fork,
                                           killThread, localTime, localTime, ms,
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

instance MonadIO m => WithNamedLogger (Core m) where
    getLoggerFromContext = liftIO $ getLoggerFromContext

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
                let safeCont x = c x `catchAll` (throwM . ContException)
                    r' = r & handlers %~ (:) (Handler handler', contHandler)
                    act = unwrapCore' r' $ m >>= wrapCore . safeCont
                    handler' e = unwrapCore' r $ handler e >>= wrapCore . c
                in  act `catches` [contHandler, Handler handler']

contHandler :: MonadThrow m => Handler m ()
contHandler = Handler $ \(ContException e) -> throwM e

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
           => ThreadCtx (Core m)
           -> (a -> Core m ())
           -> TimedT m a
           -> Core m ()
unwrapCore r c = flip runContT c
               . flip runReaderT r
               . unwrapTimedT

unwrapCore' :: Monad m => ThreadCtx (Core m) -> TimedT m () -> Core m ()
unwrapCore' r = unwrapCore r return

launchTimedT :: Monad m => TimedT m a -> m ()
launchTimedT t = flip evalStateT emptyScenario
               $ getCore
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
        nextEv <- wrapCore . Core $ do
            (ev, evs') <- fromJust . PQ.minView <$> use events
            events .= evs'
            return ev
        wrapCore . Core $ curTime .= nextEv ^. timestamp


        let ctx = nextEv ^. threadCtx
            tid = ctx ^. threadId
        keepAlive <- wrapCore $ Core $ use $ aliveThreads . to (S.member tid)
        let -- die if time has come
            maybeDie = unless keepAlive $ throwM ThreadKilled
            act      = maybeDie >> runInSandbox ctx (nextEv ^. action)
            -- catch with handlers from handlers stack
            -- catch which is performed in instance MonadCatch is not enought
            -- cause on "wait" "catch" scope finishes, we need to catch
            -- again here
        wrapCore $ (unwrapCore' ctx act) `catchesSeq` (ctx ^. handlers)

  where
    notDone :: Monad m => TimedT m Bool
    notDone = wrapCore . Core . use $ events . to (not . PQ.null)

    -- put empty continuation to an action (not our own!)
    runInSandbox r = wrapCore . unwrapCore' r

    mainThreadCtx = getNextThreadId <&>
        \tid ->
            ThreadCtx
            { _threadId = tid
            , _handlers = [( Handler $ \(SomeException e) -> throwM e
                           , Handler $ \(ContException e) -> throwM e
                           )]
            }

    -- Apply all handlers from stack.
    -- If handled ContException, ignore second handler from that layer
    catchesSeq = foldl' $ \act (h, hc) -> act `catches` [hc, h]

getNextThreadId :: Monad m => TimedT m ThreadId
getNextThreadId = wrapCore . Core $ do
    tid <- PureThreadId <$> (use threadsCounter)
    threadsCounter += 1
    aliveThreads %= S.insert tid
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

isThreadKilled :: SomeException -> Bool
isThreadKilled = maybe False (== ThreadKilled) . fromException

threadKilledNotifier :: (MonadIO m, WithNamedLogger m) => SomeException -> m ()
threadKilledNotifier e
  | isThreadKilled e = logDebug msg
  | otherwise = logWarning msg
  where
    msg = sformat ("Thread killed by exception: " % shown) e

instance (MonadIO m, MonadThrow m, MonadCatch m) => MonadTimed (TimedT m) where
    localTime = wrapCore $ Core $ use curTime
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
        wrapCore $ Core $ events %= PQ.insert Event { .. }
        return tid

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
                  \c ->
                    Core $ events %= PQ.insert (event $ c ())

    myThreadId = TimedT $ view threadId

    killThread tid = wrapCore $ Core $ aliveThreads %= S.delete tid

    -- TODO: we should probably implement this similar to
    -- http://haddock.stackage.org/lts-5.8/base-4.8.2.0/src/System-Timeout.html#timeout
    timeout t action' = do
        ref <- liftIO $ newIORef Nothing
        -- fork worker
        wtid <- fork $ do
            res <- action'
            liftIO $ writeIORef ref $ Just res
        -- wait and gather results
        waitForRes ref wtid t
      where waitForRes ref tid tout = do
                lt <- localTime
                waitForRes' ref tid $ lt + tout
            waitForRes' ref tid end = do
                tNow <- localTime
                if tNow >= end
                    then do
                        killThread tid
                        res <- liftIO $ readIORef ref
                        case res of
                            Nothing -> throwM $ MTTimeoutError "Timeout exceeded"
                            Just r -> return r
                    else do
                        wait $ for delay ms
                        res <- liftIO $ readIORef ref
                        case res of
                            Nothing -> waitForRes' ref tid end
                            Just r -> return r
            delay = 10 :: Millisecond
