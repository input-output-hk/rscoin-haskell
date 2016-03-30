-- | MessagePack serialization/deserialization for Core types

module RSCoin.Core.MessagePack
       (
       ) where

import qualified Data.ByteString.Lazy   as BSL
import           Data.Int               (Int64)
import           Data.MessagePack       (MessagePack (toObject, fromObject),
                                         Object (ObjectExt), pack, unpack)
import           Data.MessagePack.Aeson (AsMessagePack (AsMessagePack),
                                         getAsMessagePack)

import           RSCoin.Core.Aeson      ()
import           RSCoin.Core.Crypto     ()
import qualified RSCoin.Core.Primitives as C
import qualified RSCoin.Core.Types      as C

toInt :: Integral a => a -> Int
toInt = fromIntegral

fromInt :: Num a => Int -> a
fromInt = fromIntegral

-- msgpack library we use is awful :(
instance MessagePack Int64 where
    toObject = toObject . toInt
    fromObject = fmap fromInt . fromObject

instance (MessagePack a, MessagePack b) => MessagePack (Either a b) where
    toObject (Left a) = ObjectExt 0 $ BSL.toStrict $ pack a
    toObject (Right b) = ObjectExt 1 $ BSL.toStrict $ pack b
    fromObject (ObjectExt 0 a) = Left <$> unpack (BSL.fromStrict a)
    fromObject (ObjectExt 1 b) = Right <$> unpack (BSL.fromStrict b)
    fromObject _ = Nothing

instance MessagePack C.Coin where
    toObject (C.Coin c) = toObject c
    fromObject = fmap C.Coin . fromObject

instance MessagePack C.Address where
    toObject (C.Address c) = toObject c
    fromObject = fmap C.Address . fromObject

instance MessagePack C.Mintette where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.NewPeriodData where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.LBlock where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.Transaction where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.CheckConfirmation where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.HBlock where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject

instance MessagePack C.ActionLogEntry where
    toObject = toObject . AsMessagePack
    fromObject = fmap getAsMessagePack . fromObject
