-- | A small module providing necessary cryptographic functions
module RSCoin.Core.Crypto
       ( Hash
       , getHash
       , Signature
       , SecretKey
       , PublicKey
       , hash
       , sign
       , verify
       , keyGen
       ) where

import           Control.Monad.IO.Class (MonadIO)
import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Binary            (Binary (put, get), encode)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F

import           OpenSSL.RSA            (RSAKeyPair, RSAPubKey)

newtype Hash =
  Hash { getHash :: ByteString }
  deriving (Eq, Show, Binary)

instance Buildable Hash where
    build = build . F.Shown

newtype Signature =
  Signature { getSignature :: ByteString }
  deriving (Eq, Show)

newtype SecretKey =
  SecretKey { getSecretKey :: RSAKeyPair }
  deriving (Eq, Show)

newtype PublicKey =
  PublicKey { getPublicKey :: RSAPubKey }
  deriving (Eq, Show)

instance Buildable PublicKey where
    build = build . F.Shown

instance Binary PublicKey where
  get = undefined
  put = undefined

-- | Gets something serializable and gives back hash of it.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode

sign :: (MonadIO m, Binary t) => SecretKey -> t -> m Signature
sign = undefined

verify :: (MonadIO m, Binary t) => PublicKey -> t -> Signature -> m Bool
verify = undefined

keyGen :: MonadIO m => m (SecretKey, PublicKey)
keyGen = undefined
