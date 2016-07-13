module Data.Types where

import           Prelude

import           Data.Generic (class Generic, GenericSpine (..), GenericSignature (..))
import           Data.Maybe (Maybe (..))

newtype PublicKey = PublicKey String

instance showPublicKey :: Show PublicKey where
    show (PublicKey pk) = pk

instance genericPublicKey :: Generic PublicKey where
    toSpine (PublicKey pk) = SString pk
    toSignature _ = SigString
    fromSpine (SString pk) = Just $ PublicKey pk
    fromSpine _ = Nothing
