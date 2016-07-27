module Data.Types
       ( PublicKey (..)
       , Hash (..)
       , CoinAmount (..)
       ) where

import Prelude

import Data.Generic (class Generic, GenericSpine (..), GenericSignature (..))
import Data.Maybe (Maybe (..))

-- NOTE: these newtype wrappers and instances can/will be auto generated
--
-- TODO: we can ditch this module using unwrapNewtypes option, read http://www.purescript.org/learn/generic/
newtype PublicKey = PublicKey String

instance showPublicKey :: Show PublicKey where
    show (PublicKey s) = s

instance genericPublicKey :: Generic PublicKey where
    toSpine (PublicKey s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ PublicKey s
    fromSpine _ = Nothing

newtype Hash = Hash String

instance showHash :: Show Hash where
    show (Hash s) = s

instance genericHash :: Generic Hash where
    toSpine (Hash s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ Hash s
    fromSpine _ = Nothing

newtype CoinAmount = CoinAmount String

instance showRational :: Show CoinAmount where
    show (CoinAmount s) = s

instance genericCoinAmount :: Generic CoinAmount where
    toSpine (CoinAmount s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ CoinAmount s
    fromSpine _ = Nothing
