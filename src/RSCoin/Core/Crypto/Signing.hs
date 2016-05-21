{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Signing-related functions and types.

module RSCoin.Core.Crypto.Signing
       ( Signature
       , SecretKey
       , PublicKey
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

import qualified Crypto.Sign.Ed25519        as E
import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON), Value (Object),
                                             object, (.:), (.=))
import           Data.Bifunctor             (bimap)
import           Data.Binary                (Binary (get, put), decodeOrFail,
                                             encode)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import           Data.Hashable              (Hashable (hashWithSalt))
import           Data.MessagePack           (MessagePack (toObject, fromObject))
import           Data.SafeCopy              (Contained,
                                             SafeCopy (getCopy, putCopy),
                                             contain, safeGet, safePut)
import           Data.Serialize             (Get, Put)
import           Data.String                (IsString (fromString))
import           Data.Text                  (Text)
import           Data.Text.Buildable        (Buildable (build))
import           Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import qualified Data.Text.Format           as F
import qualified Data.Text.IO               as TIO
import           Data.Tuple                 (swap)
import           System.Directory           (createDirectoryIfMissing)
import           System.FilePath            (takeDirectory)

import           Serokell.Util.Exceptions   (throwText)
import           Serokell.Util.Text         (show')

import qualified RSCoin.Core.Crypto.Hashing as H

newtype Signature = Signature
    { getSignature :: E.Signature
    } deriving (Eq,Show)

instance IsString Signature where
    fromString s =
        case B64.decode (fromString s) of
            Left e -> error $ "Veritas.Core.Crypto.fromString Signature: " ++ e
            Right bs -> Signature $ E.Signature bs

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
        fmap (E.Signature . read) (v .: "signature")
    parseJSON _ = fail "Signature should be an object"

instance ToJSON Signature where
    toJSON (getSignature -> sig) =
        object ["signature" .= show (E.unSignature sig)]

instance MessagePack Signature where
    toObject = toObject . E.unSignature . getSignature
    fromObject obj = Signature . E.Signature <$> fromObject obj

instance Binary Signature where
    get = Signature . E.Signature <$> get
    put = put . E.unSignature . getSignature

newtype SecretKey = SecretKey
    { getSecretKey :: E.SecretKey
    } deriving (Eq, Show, Ord)

instance Buildable SecretKey where
    build = build . F.Shown . getSecretKey

instance Binary SecretKey where
    get = SecretKey . E.SecretKey <$> get
    put = put . E.unSecretKey . getSecretKey

instance SafeCopy SecretKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

newtype PublicKey = PublicKey
    { getPublicKey :: E.PublicKey
    } deriving (Eq, Show, Ord)

instance Buildable PublicKey where
    build = build . decodeUtf8 . B64.encode . E.unPublicKey . getPublicKey

instance Hashable PublicKey where
    hashWithSalt s pk = hashWithSalt s (encode pk)

instance Binary PublicKey where
    get = PublicKey . E.PublicKey <$> get
    put = put . E.unPublicKey . getPublicKey

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance FromJSON PublicKey where
    parseJSON (Object v) =
        PublicKey <$>
        fmap (E.PublicKey . read) (v .: "publicKey")
    parseJSON _ = fail "PublicKey should be an object"

instance ToJSON PublicKey where
    toJSON (getPublicKey -> pubKey) =
        object ["publicKey" .= show (E.unPublicKey pubKey)]

instance MessagePack PublicKey where
    toObject = toObject . show'
    fromObject obj = constructPublicKey =<< fromObject obj

-- | Sign a serializable value.
sign :: Binary t => SecretKey -> t -> Signature
sign (getSecretKey -> secKey) =
    Signature . E.dsign secKey . H.getHash . H.hash . encode

-- | Verify signature for a serializable value.
verify :: Binary t => PublicKey -> Signature -> t -> Bool
verify (getPublicKey -> pubKey) (getSignature -> sig) t =
    E.dverify pubKey (H.getHash . H.hash $ encode t) sig

-- | Generate arbitrary (secret key, public key) key pair.
keyGen :: IO (SecretKey, PublicKey)
keyGen = bimap SecretKey PublicKey . swap <$> E.createKeypair

-- | Constructs public key from base64 ByteString value
constructPublicKey :: Text -> Maybe PublicKey
constructPublicKey (encodeUtf8 -> s) =
    case B64.decode s of
        Left _ -> Nothing
        Right t -> Just $ PublicKey $ E.PublicKey t

-- | Write PublicKey to a file (base64).
writePublicKey :: FilePath -> PublicKey -> IO ()
writePublicKey fp k = do
    ensureDirectoryExists fp
    TIO.writeFile fp $ show' k

-- | Read PublicKey from a file (base64).
readPublicKey :: FilePath -> IO PublicKey
readPublicKey fp =
    maybe (throwText "Failed to parse public key") return =<<
    (constructPublicKey <$> TIO.readFile fp)

-- | Write SecretKey to a file (base64).
writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey fp (E.unSecretKey . getSecretKey -> k) = do
    ensureDirectoryExists fp
    BS.writeFile fp k

-- | Read SecretKey from a file (base64).
readSecretKey :: FilePath -> IO SecretKey
readSecretKey file = construct <$> BS.readFile file
  where
    construct s =
        case B64.decode s of
            Left e ->
                error $
                "couldn't read secret key (wrong b64 format) from " ++
                file ++ ", error: " ++ e
            Right k -> SecretKey $ E.SecretKey k

-- | Constructs secret key from file content.
-- In general it should not be used.  The only reason it's needed is
-- to read embeded key (for which we know it's correct).
constructSecretKey :: BS.ByteString -> SecretKey
constructSecretKey = SecretKey . E.SecretKey

ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists (takeDirectory -> d) =
    createDirectoryIfMissing True d

-- | Derives public key from the secret key
derivePublicKey :: SecretKey -> PublicKey
derivePublicKey (getSecretKey -> sk) =
    PublicKey $ E.toPublicKey sk

-- | Validates the sk to be the secret key of pk
checkKeyPair :: (SecretKey, PublicKey) -> Bool
checkKeyPair (sk, pk) = pk == derivePublicKey sk
