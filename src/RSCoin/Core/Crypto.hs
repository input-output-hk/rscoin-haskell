{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

-- | A small module providing necessary cryptographic functions
-- We are using secp256k1 implementation.
-- For more see wiki https://en.bitcoin.it/wiki/Secp256k1
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
       , writePublicKey
       , readPublicKey
       , writeSecretKey
       , readSecretKey
       ) where

import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Binary               (Binary (put, get), decodeOrFail,
                                            encode)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as B64
import           Data.SafeCopy             (Contained,
                                            SafeCopy (putCopy, getCopy), base,
                                            contain, deriveSafeCopy, safeGet,
                                            safePut)
import           Data.Serialize            (Get, Put)
import           Data.Text.Buildable       (Buildable (build))
import qualified Data.Text.Format          as F

import           Test.QuickCheck.Arbitrary (arbitrary)
import           Test.QuickCheck.Gen       (generate)

import           Crypto.Secp256k1          (Msg, PubKey, SecKey, Sig,
                                            derivePubKey, exportPubKey,
                                            exportSig, importPubKey, importSig,
                                            msg, signMsg, verifySig)

-- | Hash is just a base64 encoded ByteString.
newtype Hash =
    Hash { getHash :: ByteString }
    deriving (Eq, Show, Binary, Ord)

$(deriveSafeCopy 0 'base ''Hash)

instance Buildable Hash where
    build = build . F.Shown

newtype Signature =
    Signature { getSignature :: Sig }
    deriving (Eq, Show)

putCopyBinary :: Binary a => a -> Contained Put
putCopyBinary = contain . safePut . encode

getCopyBinary :: Binary a => Contained (Get a)
getCopyBinary =
    contain $
    do bs <- safeGet
       either onError onSuccess . decodeOrFail $ bs
  where
    onError (_,_,errMsg) = fail errMsg
    onSuccess (_,_,res) = return res

instance SafeCopy Signature where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance Buildable Signature where
    build = build . F.Shown

instance Binary Signature where
    get = do -- NOTE: we can implement Binary with Show/Read
        mSig <- importSig <$> get
        maybe (fail "Signature import failed") (return . Signature) mSig
    put = put . exportSig . getSignature

newtype SecretKey =
    SecretKey { getSecretKey :: SecKey }
    deriving (Eq, Show, Read)

newtype PublicKey =
    PublicKey { getPublicKey :: PubKey }
    deriving (Eq, Show, Read)

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance Buildable PublicKey where
    build = build . F.Shown

instance Binary PublicKey where
    get = do -- NOTE: we can implement Binary with Show/Read
        mKey <- importPubKey <$> get
        maybe (fail "Public key import failed") (return . PublicKey) mKey
    put = put . exportPubKey True . getPublicKey

-- | Generate a hash from a binary data.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode

-- | Generate a signature from a binary data.
sign :: Binary t => SecretKey -> t -> Signature
sign (getSecretKey -> secKey) =
    withBinaryHashedMsg $
        Signature . signMsg secKey

-- | Verify signature from a binary message data.
verify :: Binary t => PublicKey -> Signature -> t -> Bool
verify (getPublicKey -> pubKey) (getSignature -> sig) =
    withBinaryHashedMsg $
        verifySig pubKey sig

-- | Generate arbitrary (secret key, public key) key pairs.
keyGen :: IO (SecretKey, PublicKey)
keyGen = do
    sKey <- generate arbitrary
    return (SecretKey sKey, PublicKey $ derivePubKey sKey)

-- | Writes PublicKey to a file
writePublicKey :: FilePath -> PublicKey -> IO ()
writePublicKey fp = writeFile fp . show

-- | Reads PublicKey from a file
readPublicKey :: FilePath -> IO PublicKey
readPublicKey = fmap read . readFile

-- | Writes SecretKey to a file
writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey fp = writeFile fp . show

-- | Reads SecretKey from a file
readSecretKey :: FilePath -> IO SecretKey
readSecretKey = fmap read . readFile

withBinaryHashedMsg :: Binary t => (Msg -> a) -> t -> a
withBinaryHashedMsg action =
    maybe
        (error "Message is too long") -- NOTE: this shouldn't ever happen
                                      -- because SHA256.hashlazy encodes
                                      -- messages in 32 bytes
        action
        . msg . SHA256.hashlazy . encode
