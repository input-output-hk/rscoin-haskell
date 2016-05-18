{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | A small module providing necessary cryptographic functions
-- We are using secp256k1 implementation.
-- For more see wiki https://en.bitcoin.it/wiki/Secp256k1
module RSCoin.Core.Crypto
       ( Hash
       , getHash
       , Signature
       , SecretKey
       , PublicKey
       , printPublicKey
       , hash
       , sign
       , verify
       , keyGen
       , constructPublicKey
       , writePublicKey
       , readPublicKey
       , constructSecretKey
       , writeSecretKey
       , readSecretKey
       , derivePublicKey
       , checkKeyPair
       ) where

import qualified Crypto.Hash.SHA256        as SHA256
import           Data.Aeson                (FromJSON (parseJSON),
                                            ToJSON (toJSON), Value (Object),
                                            object, (.:), (.=))
import           Data.Binary               (Binary (get, put), decodeOrFail,
                                            encode)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Base64    as B64
import           Data.Hashable             (Hashable (hashWithSalt))
import           Data.MessagePack          (MessagePack (toObject, fromObject))
import           Data.Ord                  (comparing)
import           Data.SafeCopy             (Contained,
                                            SafeCopy (getCopy, putCopy), base,
                                            contain, deriveSafeCopy, safeGet,
                                            safePut)
import           Data.Serialize            (Get, Put)
import           Data.String               (IsString)
import           Data.Text                 (Text, unpack)
import           Data.Text.Buildable       (Buildable (build))
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import qualified Data.Text.Format          as F
import qualified Data.Text.IO              as TIO
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Text.Lazy            as TL
import           System.Directory          (createDirectoryIfMissing)
import           System.FilePath           (takeDirectory)
import           Test.QuickCheck.Arbitrary (Arbitrary (arbitrary))
import           Test.QuickCheck.Gen       (generate)

import           Crypto.Secp256k1          (Msg, PubKey, SecKey, Sig,
                                            derivePubKey, exportPubKey,
                                            exportSig, importPubKey, importSig,
                                            msg, signMsg, verifySig)

import           Serokell.Util.Exceptions  (throwText)
import           Serokell.Util.Text        (show')

-- | Hash is just a base64 encoded ByteString.
newtype Hash =
    Hash { getHash :: ByteString }
    deriving (Eq, Show, Binary, Ord, Hashable, MessagePack, IsString)

$(deriveSafeCopy 0 'base ''Hash)

instance Buildable Hash where
    build = build . F.Shown . getHash

instance FromJSON Hash where
    parseJSON (Object v) =
        Hash <$>
        fmap encodeUtf8 (v .: "hash")
    parseJSON _ = fail "Hash should be an object"

instance ToJSON Hash where
    toJSON (getHash -> h) = object ["hash" .= decodeUtf8 h]

newtype Signature =
    Signature { getSignature :: Sig }
    deriving (Eq, Show, IsString)

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
    build = build . F.Shown . getSignature

instance FromJSON Signature where
    parseJSON (Object v) =
        Signature <$>
        fmap read (v .: "signature")
    parseJSON _ = fail "Signature should be an object"

instance ToJSON Signature where
    toJSON (getSignature -> sig) =
        object ["signature" .= show sig]

instance MessagePack Signature where
    toObject = toObject . exportSig . getSignature
    fromObject obj = Signature <$> (importSig =<< fromObject obj)

instance Binary Signature where
    get = do
        mSig <- importSig <$> get
        maybe (fail "Signature import failed") (return . Signature) mSig
    put = put . exportSig . getSignature

newtype SecretKey =
    SecretKey { getSecretKey :: SecKey }
    deriving (Eq, Show, Read, Arbitrary)

instance Ord SecretKey where
    compare = comparing (show . getSecretKey)

instance SafeCopy SecretKey where
    putCopy = contain . safePut . show
    getCopy = contain $ read <$> safeGet

instance Buildable SecretKey where
    build = build . F.Shown . getSecretKey

newtype PublicKey =
    PublicKey { getPublicKey :: PubKey }
    deriving (Eq, Show, Read, Arbitrary)

instance FromJSON PublicKey where
    parseJSON (Object v) =
        PublicKey <$>
        fmap read (v .: "publicKey")
    parseJSON _ = fail "PublicKey should be an object"

instance ToJSON PublicKey where
    toJSON (getPublicKey -> pubKey) =
        object ["publicKey" .= show pubKey]

instance MessagePack PublicKey where
    toObject = toObject . show'
    fromObject obj = constructPublicKey =<< fromObject obj

instance Ord PublicKey where
    compare = comparing (show . getPublicKey)

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance Buildable PublicKey where
    build = build . decodeUtf8 . B64.encode . exportPubKey True . getPublicKey

instance Hashable PublicKey where
    hashWithSalt s pk = hashWithSalt s (encode pk)

instance Binary PublicKey where
    get = do
        mKey <- importPubKey <$> get
        maybe (fail "Public key import failed") (return . PublicKey) mKey
    put = put . exportPubKey True . getPublicKey

printPublicKey :: PublicKey -> String
printPublicKey = TL.unpack . toLazyText . build

-- | Generate a hash from a binary data.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode

-- | Generate a signature from a binary data.
sign :: Binary t => SecretKey -> t -> Signature
sign (getSecretKey -> secKey) =
    withBinaryHashedMsg $ Signature . signMsg secKey

-- | Verify signature from a binary message data.
verify :: Binary t => PublicKey -> Signature -> t -> Bool
verify (getPublicKey -> pubKey) (getSignature -> sig) =
    withBinaryHashedMsg $ verifySig pubKey sig

-- | Generate arbitrary (secret key, public key) key pairs.
keyGen :: IO (SecretKey, PublicKey)
keyGen = do
    sKey <- generate arbitrary
    return (SecretKey sKey, PublicKey $ derivePubKey sKey)

-- | Constructs public key from utf-8 encoded text
constructPublicKey :: Text -> Maybe PublicKey
constructPublicKey t =
    either (const Nothing) (fmap PublicKey . importPubKey) $
    B64.decode $ encodeUtf8 t

-- | Writes PublicKey to a file
writePublicKey :: FilePath -> PublicKey -> IO ()
writePublicKey fp k = do
    ensureDirectoryExists fp
    TIO.writeFile fp $ show' k

-- | Reads PublicKey from a file
readPublicKey :: FilePath -> IO PublicKey
readPublicKey fp =
    maybe (throwText "Failed to parse public key") return =<<
    (constructPublicKey <$> TIO.readFile fp)

-- | Constructs secret key from file content.
-- In general it should not be used.  The only reason it's needed is
-- to read embeded key (for which we know it's correct).
constructSecretKey :: ByteString -> SecretKey
constructSecretKey = SecretKey . read . unpack . decodeUtf8

-- | Writes SecretKey to a file
writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey fp (getSecretKey -> k) = do
    ensureDirectoryExists fp
    writeFile fp $ show k

-- | Reads SecretKey from a file
readSecretKey :: FilePath -> IO SecretKey
readSecretKey = fmap (SecretKey . read) . readFile

ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists (takeDirectory -> d) =
    createDirectoryIfMissing True d

withBinaryHashedMsg :: Binary t => (Msg -> a) -> t -> a
withBinaryHashedMsg action =
    maybe
        (error "Message is too long") -- NOTE: this shouldn't ever happen
                                      -- because SHA256.hashlazy encodes
                                      -- messages in 32 bytes
        action
        . msg . SHA256.hashlazy . encode

-- | Derives public key from the secret key
derivePublicKey :: SecretKey -> PublicKey
derivePublicKey (getSecretKey -> sk) = PublicKey $ derivePubKey sk

-- | Validates the sk to be the secret key of pk
checkKeyPair :: (SecretKey, PublicKey) -> Bool
checkKeyPair (sk, pk) = pk == derivePublicKey sk
