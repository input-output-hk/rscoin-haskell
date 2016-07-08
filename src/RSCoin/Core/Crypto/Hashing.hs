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
import           Data.Aeson                 (ToJSON (toJSON))
import           Data.Binary                (Binary, encode)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Base64     as B64
import           Data.ByteString.Lazy       (toStrict)
import           Data.Either.Combinators    (mapLeft)
import           Data.Hashable              (Hashable)
import           Data.MessagePack           (MessagePack)
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (build))
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.Format           as F

import           Serokell.Util              (show')

-- | Hash is just a ByteString.
newtype Hash =
    Hash { getHash :: ByteString }
    deriving (Eq, Show, Binary, Ord, Hashable, MessagePack, IsString)

$(deriveSafeCopy 0 'base ''Hash)

instance Buildable Hash where
    build = build . F.Shown . B64.encode . getHash

hashLengthBytes :: Integral a => a
hashLengthBytes = 256 `div` 8

parseHash :: T.Text -> Either T.Text Hash
parseHash = mapLeft T.pack . B64.decode . encodeUtf8 >=> constructHashChecked
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
    toJSON = toJSON . show'
