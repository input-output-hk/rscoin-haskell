{-# LANGUAGE TemplateHaskell #-}

-- | Hash-related functions and types.

module RSCoin.Core.Crypto.Hashing
       ( Hash
       , getHash
       , hash
       , parseHash
       ) where

import           Control.Monad              ((>=>))
import qualified Crypto.Hash.BLAKE2.BLAKE2b as BLAKE2
import           Data.Aeson                 (FromJSON (parseJSON),
                                             ToJSON (toJSON))
import           Data.Binary                (Binary, encode)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           Data.ByteString.Lazy       (toStrict)
import           Data.Hashable              (Hashable)
import           Data.MessagePack           (MessagePack)
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (build))

import qualified Serokell.Util.Base64       as B64

-- | Hash is just a ByteString.
newtype Hash = Hash
    { getHash :: ByteString
    } deriving (Eq,Show,Binary,Ord,Hashable,MessagePack,IsString)

$(deriveSafeCopy 0 'base ''Hash)

instance Buildable Hash where
    build = build . B64.encode . getHash

hashLengthBytes :: Integral a => a
hashLengthBytes = 256 `div` 8

parseHash :: T.Text -> Either T.Text Hash
parseHash = B64.decode >=> constructHashChecked
  where
    constructHashChecked bs
      | BS.length bs == hashLengthBytes = pure $ Hash bs
      | otherwise = Left "not a hash (incorrect length)"

blake2b256 :: ByteString -> ByteString
blake2b256 bs =
    let l = hashLengthBytes
    in BLAKE2.finalize l $ BLAKE2.update bs $ BLAKE2.initialize l

-- | Hash serializable data.
hash :: Binary t => t -> Hash
hash = Hash . blake2b256 . toStrict . encode

instance ToJSON Hash where
    toJSON = toJSON . B64.JsonByteString . getHash

instance FromJSON Hash where
    parseJSON = fmap (Hash . B64.getJsonByteString) . parseJSON
