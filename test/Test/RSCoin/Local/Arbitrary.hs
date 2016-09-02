{-# LANGUAGE TemplateHaskell #-}

-- | Arbitrary instances for local (non-core) types
module Test.RSCoin.Local.Arbitrary () where

import           Data.DeriveTH         (derive, makeArbitrary)
import           Test.QuickCheck       (Arbitrary (..), choose)

import           RSCoin.Core.Arbitrary ()

import           RSCoin.Mintette.Error (MintetteError (..))
import           RSCoin.Notary.Error   (NotaryError (..))

derive makeArbitrary ''MintetteError
derive makeArbitrary ''NotaryError
