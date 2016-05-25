{-# LANGUAGE TemplateHaskell #-}

-- | Hash-related functions and types.

module RSCoin.Core.Crypto.Hashing
       ( Hash
       , getHash
       , hash
       ) where

import qualified Crypto.Hash.SHA256     as SHA256
import           Data.Binary            (Binary, encode)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as B64
import           Data.Hashable          (Hashable)
import           Data.MessagePack       (MessagePack)
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Data.String            (IsString)
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F

-- | Hash is just a base64 encoded ByteString.
newtype Hash =
    Hash { getHash :: ByteString }
    deriving (Eq, Show, Binary, Ord, Hashable, MessagePack, IsString)

$(deriveSafeCopy 0 'base ''Hash)

instance Buildable Hash where
    build = build . F.Shown . getHash

-- | Hash serializable data.
hash :: Binary t => t -> Hash
hash = Hash . B64.encode . SHA256.hashlazy . encode
