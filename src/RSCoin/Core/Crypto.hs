{-# LANGUAGE ViewPatterns #-}
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

import           Crypto.Secp256k1       (SecKey, PubKey, Sig,
                                        exportPubKey, importPubKey,
                                        importSig, exportSig, signMsg)

newtype Hash =
  Hash { getHash :: ByteString }
  deriving (Eq, Show, Binary)

instance Buildable Hash where
    build = build . F.Shown

newtype Signature =
  Signature { getSignature :: Sig }
  deriving (Eq, Show)

instance Buildable Signature where
    build = build . F.Shown

instance Binary Signature where
  get = do
    mSig <- importSig <$> get
    return $ maybe (error "Signature import failed") Signature mSig
  put = put . exportSig . getSignature

newtype SecretKey =
  SecretKey { getSecretKey :: SecKey }
  deriving (Eq, Show)

newtype PublicKey =
  PublicKey { getPublicKey :: PubKey }
  deriving (Eq, Show)

instance Buildable PublicKey where
    build = build . F.Shown

instance Binary PublicKey where
  get = do
    mKey <- importPubKey <$> get
    return $ maybe (error "Public key import failed") PublicKey mKey
  put = put . exportPubKey True . getPublicKey

-- | Gets something serializable and gives back hash of it.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode

sign :: Binary t => SecretKey -> t -> Signature
sign = undefined -- (getSecretKey -> secKey) = Signature . signMsg secKey

verify :: (MonadIO m, Binary t) => PublicKey -> Signature -> t -> m Bool
verify = undefined

keyGen :: MonadIO m => m (SecretKey, PublicKey)
keyGen = undefined
