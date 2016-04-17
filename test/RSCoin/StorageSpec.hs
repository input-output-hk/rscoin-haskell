{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Storage.

module RSCoin.StorageSpec
       ( spec
       ) where

import           Control.Exception          (Exception)
import           Control.Monad              (void)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Bank.Error         as B
import qualified RSCoin.Mintette.Error     as M
import qualified RSCoin.Core               as C

import           RSCoin.Core.Arbitrary       ()
import qualified RSCoin.Core.Storage         as T
import qualified RSCoin.Bank.StorageSpec     as B
import qualified RSCoin.Mintette.StorageSpec as M

spec :: Spec
spec =
    describe "Storage" $ do
        return ()

data RSCoinState =
    RSCoinState { bankState      :: B.StorageAndKey
                , mintettesState :: [M.StorageAndKey]
                }

type Update = T.Update C.RSCoinError RSCoinState
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
