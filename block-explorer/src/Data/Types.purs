module Data.Types
       ( PublicKey (..)
       , Hash (..)
       , CoinAmount (..)
       , TransactionId
       ) where

import Prelude

import Data.Generic (class Generic)

import Data.Maybe (Maybe (..))
import Data.Time  (Second)

-- NOTE: these newtype wrappers and instances can/will be auto generated
--
-- TODO: we can ditch this module using unwrapNewtypes option, read http://www.purescript.org/learn/generic/
newtype PublicKey = PublicKey String

derive instance eqPublicKey :: Eq PublicKey

instance showPublicKey :: Show PublicKey where
    show (PublicKey s) = s

derive instance genericPublicKey :: Generic PublicKey

newtype Hash = Hash String
type TransactionId = Hash

derive instance eqHash :: Eq Hash

instance showHash :: Show Hash where
    show (Hash s) = s

derive instance genericHash :: Generic Hash

newtype CoinAmount = CoinAmount Number

instance showCoinAmount :: Show CoinAmount where
    show (CoinAmount s) = show s

derive instance genericCoinAmount :: Generic CoinAmount
