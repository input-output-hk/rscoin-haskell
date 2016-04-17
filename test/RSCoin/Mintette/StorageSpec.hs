{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Mintette's Storage.

module RSCoin.Mintette.StorageSpec
       ( spec
       ) where

import           Control.Exception          (throw, fromException, Exception)
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow (throwM), SomeException (..))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.State        (runState, State)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Mintette.Error     as S
import qualified RSCoin.Mintette.Storage   as S
import qualified RSCoin.Core               as C

import           RSCoin.Core.Arbitrary ()
import qualified RSCoin.Core.Storage       as T

spec :: Spec
spec =
    describe "Mintette storage" $ do
        return ()

type Update = T.Update S.MintetteError S.Storage
type UpdateVoid = Update ()

class CanUpdate a where
    doUpdate :: a -> UpdateVoid

data SomeUpdate = forall a . CanUpdate a => SomeUpdate a

data EmptyUpdate = EmptyUpdate
    deriving Show

instance Arbitrary EmptyUpdate where
    arbitrary = pure EmptyUpdate

instance CanUpdate EmptyUpdate where
    doUpdate _ = return ()
