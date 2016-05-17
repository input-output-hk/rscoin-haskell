{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | WIP

module Test.RSCoin.Pure.Update
       ( Update (..)
       , execUpdate
       , runUpdate
       , execUpdateSafe
       , runUpdateSafe
       ) where

import           Control.Exception          (Exception, fromException, throw)
import           Control.Monad.Catch        (MonadThrow (throwM),
                                             SomeException (..))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.Reader       (MonadReader (ask, local))
import           Control.Monad.State        (State, get, modify, runState)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans.Except (ExceptT, throwE)

newtype Update e s a =
    Update { getUpdate :: ExceptT e (State s) a }
    deriving (MonadState s, Monad, Applicative, Functor)

instance MonadReader s (Update e s) where
    ask = get
    local f m = modify f >> m

instance Exception e => MonadThrow (Update e s) where
    throwM e = Update . maybe (throw e') throwE $ fromException e'
      where
        e' = SomeException e

execUpdate :: Exception e => Update e s a -> s -> s
execUpdate u = snd . runUpdate u

runUpdate :: Exception e => Update e s a -> s -> (a, s)
runUpdate upd storage = either throw (, newStorage) res
  where
    (res, newStorage) = runState (runExceptT $ getUpdate upd) storage

execUpdateSafe :: (MonadThrow m, Exception e) => Update e s a -> s -> m s
execUpdateSafe upd storage = snd <$> runUpdateSafe upd storage

runUpdateSafe :: (MonadThrow m, Exception e) => Update e s a -> s -> m (a, s)
runUpdateSafe upd storage = either throwM (return . (, newStorage)) res
  where
    (res, newStorage) = runState (runExceptT $ getUpdate upd) storage
