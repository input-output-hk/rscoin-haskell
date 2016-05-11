{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | WIP

module Test.RSCoin.Pure.BankState
       ( Update
       , UpdateVoid
       , BankState (..)
       , bankStorage
       , bankKey
       ) where

import           Control.Lens               (makeLenses)

import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             frequency)

import qualified RSCoin.Bank.Error          as S
import qualified RSCoin.Bank.Storage        as S
import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()
import qualified Test.RSCoin.Pure.Update    as T

type Update = T.Update S.BankError S.Storage
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

data AddMintette = AddMintette C.Mintette C.PublicKey

instance Arbitrary AddMintette where
  arbitrary = AddMintette <$> arbitrary <*> arbitrary

instance CanUpdate AddMintette where
    doUpdate (AddMintette m k) = S.addMintette m k

instance Arbitrary SomeUpdate where
    arbitrary =
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))]

data BankState = BankState
    { _bankStorage :: S.Storage
    , _bankKey     :: C.SecretKey
    }

$(makeLenses ''BankState)

instance Show BankState where
  show = const "BankState"

instance Arbitrary BankState where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . flip BankState sk $ T.execUpdate (doUpdate upd) S.mkStorage
