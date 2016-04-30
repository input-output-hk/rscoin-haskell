{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module contains time management monad and it's implementation for IO.
module RSCoin.Test.MonadTimed
    ( fork, wait, localTime, workWhile, work, schedule, invoke, timeout
    , minute , sec , ms , mcs
    , minute', sec', ms', mcs'
    , tu
    , at, after, for, till, now
    , during, upto
    , MicroSeconds
    , MonadTimed
    , RelativeToNow
    ) where

import           Control.Monad.Catch  (MonadThrow)
import           Control.Monad.Trans  (lift)
import           Control.Monad.Reader (ReaderT(..), runReaderT, ask)
import           Control.Monad.State  (StateT, evalStateT, get)

import           Data.Time.Units      (TimeUnit, toMicroseconds)

type MicroSeconds = Int

-- | Defines some time point (relative to current time point) 
--   basing on current time point
type RelativeToNow = MicroSeconds -> MicroSeconds

-- | Allows time management. Time is specified in microseconds passed
--   from start point (origin).
class MonadThrow m => MonadTimed m where
    -- | Acquires time relative to origin point
    localTime :: m MicroSeconds

    -- | Creates another thread of execution, with same point of origin
    fork :: m () -> m ()
    fork = workWhile $ return True

    -- | Waits till specified relative time
    wait :: RelativeToNow -> m ()

    -- | Forks a temporal thread, which exists
    --   until preficate evaluates to False
    workWhile :: m Bool -> m () -> m ()

    -- | Throws an TimeoutError exception if running an action exceeds running time
    timeout :: MicroSeconds -> m a -> m a

-- | Executes an action somewhere in future
schedule :: MonadTimed m => RelativeToNow -> m () -> m ()
schedule time action = fork $ wait time >> action

-- | Executes an action at specified time in current thread
invoke :: MonadTimed m => RelativeToNow -> m a -> m a
invoke time action = wait time >> action

-- | Like workWhile, unwraps first layer of monad immediatelly
--   and then checks predicate periocially
work :: MonadTimed m => m (m Bool) -> m () -> m ()
work predicate action = predicate >>= \p -> workWhile p action

instance MonadTimed m => MonadTimed (ReaderT r m) where
    localTime = lift localTime

    wait = lift . wait

    fork m = lift . fork . runReaderT m =<< ask

    workWhile p m =
        lift . (workWhile <$> runReaderT p <*> runReaderT m) =<< ask

    timeout t m = lift . timeout t . runReaderT m =<< ask

instance MonadTimed m => MonadTimed (StateT r m) where
    localTime = lift localTime

    wait = lift . wait

    fork m = lift . fork . evalStateT m =<< get

    workWhile p m =
        lift . (workWhile <$> evalStateT p <*> evalStateT m) =<< get

    timeout t m = lift . timeout t . evalStateT m =<< get

-- * Some usefull functions below

-- | Defines measure for time periods
mcs, ms, sec, minute :: Int -> MicroSeconds
mcs    = id
ms     = (*) 1000
sec    = (*) 1000000
minute = (*) 60000000

mcs', ms', sec', minute' :: Double -> MicroSeconds
mcs'    = round
ms'     = round . (*) 1000
sec'    = round . (*) 1000000
minute' = round . (*) 60000000

tu :: TimeUnit t => t -> MicroSeconds
tu  =  fromIntegral . toMicroseconds

-- | Time point by given absolute time (still relative to origin)
at, till :: TimeAcc t => t
at   = at' 0
till = at' 0

-- | Time point relative to current time
after, for :: TimeAcc t => t
after = after' 0
for   = after' 0

-- | Current time point
now :: RelativeToNow
now = const 0

-- | Returns whether specified delay has passed
--   (timer starts when first monad layer is unwrapped)
during :: MonadTimed m => MicroSeconds -> m (m Bool)
during time = do
    end <- (time + ) <$> localTime
    return $ (end > ) <$> localTime

-- | Returns whether specified time point has passed
upto :: MonadTimed m => MicroSeconds -> m (m Bool)
upto time = return $ (time > ) <$> localTime

-- black magic
class TimeAcc t where
    at'    :: MicroSeconds -> t
    after' :: MicroSeconds -> t

instance TimeAcc RelativeToNow where
    at'    = (-)
    after' = const

instance (a ~ b, TimeAcc t) => TimeAcc (a -> (b -> MicroSeconds) -> t) where
    at'    acc t f = at'    $ f t + acc
    after' acc t f = after' $ f t + acc
