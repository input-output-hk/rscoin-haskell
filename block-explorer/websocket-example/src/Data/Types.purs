module Data.Types where

import Prelude

import Data.Generic (class Generic, GenericSpine (..), GenericSignature (..))
import Data.Maybe (Maybe (..))

-- NOTE: these newtype wrappers and instances can/will be auto generated
--
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

newtype Rational = Rational String

instance showRational :: Show Rational where
    show (Rational s) = s

instance genericRational :: Generic Rational where
    toSpine (Rational s) = SString s
    toSignature _ = SigString
    fromSpine (SString s) = Just $ Rational s
    fromSpine _ = Nothing
