-- | A small module providing necessary cryptographic functions
module RSCoin.Core.Crypto
       ( Hash
       , getHash
       , Signature
       , getSignature
       , SecretKey
       , PublicKey
       , hash
       , sign
       , verify
       , keyGen
       ) where

import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Binary            (Binary, encode)
import qualified Crypto.Hash.SHA256     as SHA256

import           OpenSSL.EVP.PKey       (SomeKeyPair, SomePublicKey)

newtype Hash = Hash { getHash :: ByteString }
newtype Signature = Signature { getSignature :: ByteString }

newtype SecretKey  = SecretKey { getSecretKey :: SomeKeyPair }
newtype PublicKey = PublicKey { getPublicKey :: SomePublicKey }

-- | Gets something serializable and gives back hash of it.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode

sign :: Binary t => SecretKey -> t -> IO Signature
sign = undefined

verify :: Binary t => PublicKey -> t -> Signature -> IO Bool
verify = undefined

keyGen :: IO (SecretKey, PublicKey)
keyGen = undefined
