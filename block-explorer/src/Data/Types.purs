module Data.Types
       ( PublicKey (..)
       , Hash (..)
       , CoinAmount (..)
       , TransactionId
       ) where

import Prelude

import Data.Generic (class Generic, GenericSpine (..), GenericSignature (..))

import Data.Maybe (Maybe (..))

-- NOTE: these newtype wrappers and instances can/will be auto generated
--
-- TODO: we can ditch this module using unwrapNewtypes option, read http://www.purescript.org/learn/generic/
newtype PublicKey = PublicKey String

derive instance eqPublicKey :: Eq PublicKey

instance showPublicKey :: Show PublicKey where
    show (PublicKey s) = s

instance genericPublicKey :: Generic PublicKey where
    toSpine (PublicKey s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ PublicKey s
    fromSpine _ = Nothing

newtype Hash = Hash String
type TransactionId = Hash

derive instance eqHash :: Eq Hash

instance showHash :: Show Hash where
    show (Hash s) = s

instance genericHash :: Generic Hash where
    toSpine (Hash s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ Hash s
    fromSpine _ = Nothing

newtype CoinAmount = CoinAmount Number

instance showCoinAmount :: Show CoinAmount where
    show (CoinAmount s) = show s

instance genericCoinAmount :: Generic CoinAmount where
    toSpine (CoinAmount s) = SNumber s
    toSignature _ = SigNumber
    fromSpine (SNumber s) = Just $ CoinAmount s
    fromSpine _ = Nothing
