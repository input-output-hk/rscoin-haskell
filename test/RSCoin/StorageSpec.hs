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
                                            uses, (%=), (&), (+=), (.=), (.~), at)

import           Control.Monad              (forM_, guard, unless, void)
import           Control.Monad.Trans        (lift)
import           Control.Exception          (Exception)
import           Control.Monad.State.Lazy   (modify, gets)
import qualified Data.Map                   as M
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
    RSCoinState { _bankState      :: B.BankState
                , _mintettesState :: M.Map C.Mintette M.MintetteState
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

instance CanUpdate AddMintette where
    doUpdate (AddMintette m (sk, pk)) = do
        liftBankUpdate $ B.addMintette m pk

-- instance Arbitrary RSCoinState where
--     arbitrary = do
--         bank <- arbitrary
--         SomeUpdate upd <- arbitrary
--         return . T.execUpdate (doUpdate upd) $ RSCoinState bank []

liftBankUpdate :: T.Update B.BankError B.Storage () -> T.Update C.RSCoinError RSCoinState ()
liftBankUpdate upd = do
    bank <- gets $ B._bankStorage . _bankState
    newBank <- T.execUpdateSafe upd bank
    bankState . B.bankStorage .= newBank

liftMintetteUpdate :: C.Mintette -> T.Update M.MintetteError M.Storage () -> T.Update C.RSCoinError RSCoinState ()
liftMintetteUpdate mintette upd = do
    mMintette <- gets (fmap M._mintetteStorage . M.lookup mintette . _mintettesState)
    mNewStorage <- return $ mMintette >>= T.execUpdateSafe upd 
    maybe (return ()) (mintettesState . ix mintette . M.mintetteStorage .=) mNewStorage
