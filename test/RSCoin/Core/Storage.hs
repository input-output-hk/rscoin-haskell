{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Bank and Mintette common storage types

module RSCoin.Core.Storage
       ( Update (..)
       , execUpdate
       , runUpdate
       ) where

import           Control.Exception          (throw, fromException, Exception)
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow (throwM), SomeException (..))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.State        (runState, State)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Core           as C

import           RSCoin.Core.Arbitrary ()


newtype Update e s a =
    Update { getUpdate :: ExceptT e (State s) a }
    deriving (MonadState s, Monad, Applicative, Functor)

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
