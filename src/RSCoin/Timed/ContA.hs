{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE UndecidableInstances      #-}

module RSCoin.Test.ContA
    ( ContAT
    , contAT
    , runContAT
    , ContA
    , contA
    , runContA
    ) where

import           Control.Monad          (ap, liftM)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.Cont     (ContT (..), cont, runCont)
import           Control.Monad.Identity (Identity)
import           Control.Monad.State    (MonadState, get, put, state)
import           Control.Monad.Trans    (MonadIO, MonadTrans, lift, liftIO)


-- | Like Cont monad, but forall-quantificated by it's return type.
--   It might be helpful in resolving
--   "TimedT doesn't return a value" non-feature

newtype ContAT m a = ContAT { runContAT' :: forall r . ContT r m a }

contAT :: (forall r . (a -> m r) -> m r) -> ContAT m a
contAT f = ContAT $ ContT f

runContAT :: ContAT m a -> forall r . (a -> m r) -> m r
runContAT m = runContT $ runContAT' m

type ContA = ContAT Identity

contA :: (forall r . (a -> r) -> r) -> ContA a
contA m = ContAT $ cont m

runContA :: ContA a -> (a -> r) -> r
runContA (ContAT x) c = runCont x c


-- * Base monad instances

instance Monad m => Functor (ContAT m) where
    fmap = liftM

instance Monad m => Applicative (ContAT m) where
    pure = return
    (<*>) = ap

instance Monad m => Monad (ContAT m) where
    return x = ContAT $ return x

    ContAT m >>= f = ContAT $ m >>= runContAT' . f


-- * Auxilary monad instances

instance MonadTrans ContAT where
    lift x = ContAT $ lift x

instance MonadState s m => MonadState s (ContAT m) where
    get = ContAT get
    put x = ContAT $ put x
    state s = ContAT $ state s

instance MonadIO m => MonadIO (ContAT m) where
    liftIO x = ContAT $ liftIO x

instance MonadThrow m => MonadThrow (ContAT m) where
    throwM e = ContAT $ throwM e
