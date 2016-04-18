{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Storage.

module RSCoin.StorageSpec
       ( spec
       ) where

import           Control.Lens              (Getter, ix, makeLenses, to, use,
                                            uses, (%=), (&), (+=), (.=), (.~), _1)

import           Control.Monad             (forM_, guard, unless)
import           Control.Exception          (Exception)
import           Control.Monad              (void)
import           Control.Monad.State.Lazy   (modify)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Bank.Error     as B
import qualified RSCoin.Mintette.Error as M
import qualified RSCoin.Bank.Storage     as B
import qualified RSCoin.Mintette.Storage as M
import qualified RSCoin.Core             as C

import           RSCoin.Core.Arbitrary       ()
import qualified RSCoin.Core.Storage         as T
import qualified RSCoin.Bank.StorageSpec     as B
import qualified RSCoin.Mintette.StorageSpec as M

spec :: Spec
spec =
    describe "Storage" $ do
        return ()

data RSCoinState =
    RSCoinState { _bankState      :: B.StorageAndKey
                , _mintettesState :: [M.StorageAndKey]
                }

$(makeLenses ''RSCoinState)

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

data AddMintette = AddMintette C.Mintette (C.SecretKey, C.PublicKey)
    deriving Show

instance Arbitrary AddMintette where
  arbitrary = do
    mintette <- arbitrary
    sk <- arbitrary
    return $ AddMintette mintette (sk, C.derivePublicKey sk)

liftUpdate :: T.Update B.BankError B.Storage () -> T.Update C.RSCoinError RSCoinState ()
liftUpdate upd =
    bankState . B.getStorageAndKey . _1 %= T.execUpdate upd

instance CanUpdate AddMintette where
    doUpdate (AddMintette m (sk, pk)) = do
        liftUpdate $ B.addMintette m pk

-- instance Arbitrary RSCoinState where
--     arbitrary = do
--         bank <- arbitrary
--         SomeUpdate upd <- arbitrary
--         return . T.execUpdate (doUpdate upd) $ RSCoinState bank []
