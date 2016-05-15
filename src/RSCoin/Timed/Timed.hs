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

import Data.Maybe (isJust)
import Debug.Trace


type Timestamp = Microsecond

-- | Private context for each pure thread
data ThreadCtx c = ThreadCtx
    { _threadId :: ThreadId        -- ^ Thread id
    , _handlers :: [Handler c ()]  -- ^ Exception handlers stack
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
                let remE = \x -> (liftIO $ putStrLn ("@Rem er" ++ show x)) >> (liftIO . writeIORef contExcept . Just $ x)
                    act = unwrapCore' r' $ m >>= \x -> wrapCore $ (getCore $ c x) `catchAll` remE
                    handler' e = unwrapCore' r $ (liftIO . putStrLn) ("@handled" ++ show e) >> handler e >>= wrapCore . getCore . c
                    r' = r & handlers %~ (Handler (Core . handler') : )
                Core $ act `catch` handler'
                e <- liftIO $ readIORef contExcept
                liftIO $ putStrLn $ "Var exc: " ++ show e
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
        wrapCore . getCore $ (Core (unwrapCore' ctx act)) `catches` (ctx ^. handlers)

  where
    notDone :: Monad m => TimedT m Bool
    notDone = TimedT . lift . lift . Core . use $ events . to (not . PQ.null)

    -- put empty continuation to an action (not our own!)
    runInSandbox r = wrapCore . unwrapCore' r

    mainThreadCtx = getNextThreadId <&>
        \tid ->
            ThreadCtx
            { _threadId = tid
            , _handlers = [Handler threadKilledNotifier]
            }

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
    liftIO $ putStrLn "initiating"
    runTimedT $ do
        liftIO $ putStrLn "!launching"
        m <- try timed
        liftIO $ putStrLn "!!!!"
        liftIO $ putStrLn $ either (const "!Left") (const "!Right") m
        liftIO $ putStrLn "???"
        liftIO . writeIORef ref . Just $ m
        liftIO $ putStrLn "!wrote"
    -- traceM $ maybe "!Nothing" (const "!Just") res
    res :: Either SomeException a <- fromJust <$> (liftIO (readIORef ref) >>= \x -> liftIO (print $ isJust x) >> return x)
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
                , _handlers = [Handler threadKilledNotifier]
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
