{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Signing-related functions and types.

module RSCoin.Core.Crypto.Signing
       ( Signature
       , SecretKey
       , PublicKey
       , sign
       , verify
       , verifyChain
       , keyGen
       , deterministicKeyGen
       , constructSignature
       , constructPublicKey
       , writePublicKey
       , readPublicKey
       , constructSecretKey
       , writeSecretKey
       , readSecretKey
       , derivePublicKey
       , checkKeyPair
       , printPublicKey
       ) where

import qualified Crypto.Sign.Ed25519          as E
import           Data.Aeson                   (FromJSON (parseJSON),
                                               ToJSON (toJSON))
import           Data.Bifunctor               (bimap)
import           Data.Binary                  (Binary (get, put), decodeOrFail,
                                               encode)
import qualified Data.ByteString              as BS
import           Data.Char                    (isSpace)
import           Data.Hashable                (Hashable (hashWithSalt))
import           Data.Maybe                   (fromMaybe)
import           Data.MessagePack             (MessagePack (fromObject, toObject))
import           Data.SafeCopy                (Contained,
                                               SafeCopy (getCopy, putCopy),
                                               contain, safeGet, safePut)
import           Data.Serialize               (Get, Put)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Buildable          (Buildable (build))
import qualified Data.Text.IO                 as TIO
import qualified Data.Text.Lazy               as TL
import           Data.Text.Lazy.Builder       (toLazyText)
import           Data.Tuple                   (swap)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory)
import           Test.QuickCheck              (Arbitrary (arbitrary), vector)
import           Text.ParserCombinators.ReadP (between, char, munch1,
                                               skipSpaces, string)
import           Text.Read                    (lift, readPrec)

import qualified Serokell.Util.Base64         as B64
import           Serokell.Util.Exceptions     (throwText)
import           Serokell.Util.Text           (listBuilderJSON, pairBuilder,
                                               show')

import qualified RSCoin.Core.Crypto.Hashing   as H

newtype Signature a = Signature
    { getSignature :: E.Signature
    } deriving (Eq, Ord)

sigToBs :: (Signature a) -> BS.ByteString
sigToBs = E.unSignature . getSignature

bsToSig :: BS.ByteString -> (Signature a)
bsToSig = Signature . E.Signature

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

instance SafeCopy (Signature a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance Buildable (Signature a, PublicKey) where
    build = pairBuilder

instance Buildable [(Signature a, PublicKey)] where
    build = listBuilderJSON

instance Buildable (Signature a) where
    build = build . B64.encode . E.unSignature . getSignature

-- @TODO: avoid code duplication for Show and Read instances
instance Show (Signature a) where
    show sig = "Signature { getSignature = " ++ T.unpack (show' sig) ++ " }"

instance Read (Signature a) where
    readPrec = lift $
        either
            (error . T.unpack)
            (Signature . E.Signature)
        . B64.decode
        . T.pack
        <$>
        (string "Signature " *> between (char '{') (char '}')
          (string " getSignature = " *> munch1 (not . isSpace) <* skipSpaces))

instance MessagePack (Signature a) where
    toObject = toObject . sigToBs
    fromObject obj = bsToSig <$> fromObject obj

instance Binary (Signature a) where
    get = bsToSig <$> get
    put = put . sigToBs

instance ToJSON (Signature a)where
    toJSON = toJSON . B64.encode . sigToBs

instance FromJSON (Signature a) where
    parseJSON = fmap (bsToSig . B64.getJsonByteString) . parseJSON


newtype SecretKey = SecretKey
    { getSecretKey :: E.SecretKey
    } deriving (Eq, Ord)

instance Buildable SecretKey where
    build = build . B64.encode . E.unSecretKey . getSecretKey

instance Show SecretKey where
    show sk = "SecretKey { getSecretKey = " ++ T.unpack (show' sk) ++ " }"

instance Read SecretKey where
    readPrec = lift $
        either
            (error . T.unpack)
            (SecretKey . E.SecretKey)
        . B64.decode
        . T.pack
        <$>
        (string "SecretKey " *> between (char '{') (char '}')
          (string " getSecretKey = " *> munch1 (not . isSpace) <* skipSpaces))

instance Binary SecretKey where
    get = SecretKey . E.SecretKey <$> get
    put = put . E.unSecretKey . getSecretKey

instance SafeCopy SecretKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance Arbitrary SecretKey where
    arbitrary =
        SecretKey .
        snd .
        fromMaybe (error "createKeypairFromSeed_ failed") .
        E.createKeypairFromSeed_ . BS.pack <$>
        vector 32

newtype PublicKey = PublicKey
    { getPublicKey :: E.PublicKey
    } deriving (Eq, Ord)

pkToBs :: PublicKey -> BS.ByteString
pkToBs = E.unPublicKey . getPublicKey

bsToPk :: BS.ByteString -> PublicKey
bsToPk = PublicKey . E.PublicKey

instance Buildable PublicKey where
    build = build .  B64.encode . pkToBs

instance Show PublicKey where
    show pk = "PublicKey { getPublicKey = " ++ T.unpack (show' pk) ++ " }"

instance Read PublicKey where
    readPrec = lift $
        either
            (error . T.unpack)
            (PublicKey . E.PublicKey)
        . B64.decode
        . T.pack
        <$>
        (string "PublicKey " *> between (char '{') (char '}')
          (string " getPublicKey = " *> munch1 (not . isSpace) <* skipSpaces))

instance Hashable PublicKey where
    hashWithSalt s = hashWithSalt s . E.unPublicKey . getPublicKey

instance Binary PublicKey where
    get = bsToPk <$> get
    put = put . pkToBs

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary

instance MessagePack PublicKey where
    toObject = toObject . pkToBs
    fromObject = fmap bsToPk . fromObject

instance Arbitrary PublicKey where
    arbitrary = derivePublicKey <$> arbitrary

instance ToJSON PublicKey where
    toJSON = toJSON . B64.encode . pkToBs

instance FromJSON PublicKey where
    parseJSON = fmap (bsToPk . B64.getJsonByteString) . parseJSON

-- | Sign a serializable value.
sign :: Binary t => SecretKey -> t -> Signature t
sign (getSecretKey -> secKey) =
    Signature . E.dsign secKey . H.getHash . H.hash

-- | Verify signature for a serializable value.
verify :: Binary t => PublicKey -> Signature t -> t -> Bool
verify (getPublicKey -> pubKey) (getSignature -> sig) t =
    E.dverify pubKey (H.getHash $ H.hash t) sig

-- | Verify chain of certificates.
verifyChain :: PublicKey -> [(Signature PublicKey, PublicKey)] -> Bool
verifyChain _ [] = True
verifyChain pk ((sig, nextPk):rest) = verify pk sig nextPk && verifyChain nextPk rest

-- | Generate arbitrary (secret key, public key) key pair.
keyGen :: IO (SecretKey, PublicKey)
keyGen = bimap SecretKey PublicKey . swap <$> E.createKeypair

-- | Creates key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> Maybe (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey <$> E.createKeypairFromSeed_ seed

-- | Constructs signature from UTF-8 base64 text.
constructSignature :: Text -> Maybe (Signature a)
constructSignature =
    either (const Nothing) (Just . Signature . E.Signature) . B64.decode . trim
  where
    trim = T.dropAround isSpace

-- | Constructs public key from UTF-8 base64 text.
constructPublicKey :: Text -> Maybe PublicKey
constructPublicKey =
    either (const Nothing) (Just . PublicKey . E.PublicKey) . B64.decode . trim
  where
    trim = T.dropAround isSpace

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

-- | Write SecretKey to a file (binary format).
writeSecretKey :: FilePath -> SecretKey -> IO ()
writeSecretKey fp (E.unSecretKey . getSecretKey -> k) = do
    ensureDirectoryExists fp
    BS.writeFile fp k

-- | Read SecretKey from a file (binary format).
readSecretKey :: FilePath -> IO SecretKey
readSecretKey file = SecretKey . E.SecretKey <$> BS.readFile file

-- | Construct secret key from file content.
-- In general it should not be used.  The only reason it's needed is
-- to read embeded key (for which we know it's correct).
constructSecretKey :: BS.ByteString -> SecretKey
constructSecretKey = SecretKey . E.SecretKey

ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists (takeDirectory -> d) =
    createDirectoryIfMissing True d

-- | Derive public key from the secret key
derivePublicKey :: SecretKey -> PublicKey
derivePublicKey (getSecretKey -> sk) =
    PublicKey $ E.toPublicKey sk

-- | Validate the sk to be the secret key of pk
checkKeyPair :: (SecretKey, PublicKey) -> Bool
checkKeyPair (sk, pk) = pk == derivePublicKey sk

-- | String representation of key
printPublicKey :: PublicKey -> String
printPublicKey = TL.unpack . toLazyText . build
