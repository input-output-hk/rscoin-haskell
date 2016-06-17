{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- | MessagePack serialization/deserialization for Core types

module RSCoin.Core.MessagePack
       (
       ) where

import           Data.Binary            (encode, decodeOrFail)
import qualified Data.ByteString.Lazy   as BSL
import           Data.Int               (Int64)
import           Data.MessagePack       (MessagePack (toObject, fromObject),
                                         Object (ObjectExt, ObjectBin, ObjectInt), pack, unpack)
import           Data.Ratio             (Ratio, numerator, denominator, (%))
import           Data.Tuple.Select      (sel3)
import           Data.Tuple.Curry       (uncurryN)

import           RSCoin.Core.Crypto     ()
import qualified RSCoin.Core.Primitives as C
import qualified RSCoin.Core.Types      as C

toInt :: Integral a => a -> Int
toInt = fromIntegral

fromInt :: Num a => Int -> a
fromInt = fromIntegral

uncurry2 :: (a -> b -> c) -> (a, b) -> c
uncurry2 = uncurryN

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 = uncurryN

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 = uncurryN

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 = uncurryN

-- msgpack library we use is awful :(
-- RЕАЛLY IT"S SО AWFUЛ
instance MessagePack Int64 where
    toObject = toObject . toInt
    fromObject = fmap fromInt . fromObject

instance MessagePack Integer where
    toObject i
        | fromInt minBound <= i && i <= fromInt maxBound = ObjectInt $ toInt i
        | otherwise = ObjectBin . BSL.toStrict $ encode i
    fromObject (ObjectInt i) = Just $ fromInt i
    fromObject (ObjectBin b) =
        either (const Nothing) (Just . sel3) . decodeOrFail $ BSL.fromStrict b
    fromObject _             = Nothing

instance (Integral a, MessagePack a) => MessagePack (Ratio a) where
    toObject r = toObject (numerator r, denominator r)
    fromObject = fmap (uncurry (%)) . fromObject

instance (MessagePack a, MessagePack b) => MessagePack (Either a b) where
    toObject (Left a) = ObjectExt 0 $ BSL.toStrict $ pack a
    toObject (Right b) = ObjectExt 1 $ BSL.toStrict $ pack b
    fromObject (ObjectExt 0 a) = Left <$> unpack (BSL.fromStrict a)
    fromObject (ObjectExt 1 b) = Right <$> unpack (BSL.fromStrict b)
    fromObject _ = Nothing

instance MessagePack C.Coin where
    toObject (C.Coin c t) = toObject (c, t)
    fromObject = fmap (uncurry C.Coin) . fromObject

instance MessagePack C.Address where
    toObject (C.Address c) = toObject c
    fromObject = fmap C.Address . fromObject

instance MessagePack C.Mintette where
    toObject C.Mintette{..} =
        toObject (toObject mintetteHost, toObject mintettePort)
    fromObject = fmap (uncurry2 C.Mintette) . fromObject

instance MessagePack C.NewPeriodData where
    toObject C.NewPeriodData{..} =
        toObject
            (npdPeriodId, npdMintettes, npdHBlock, npdNewIdPayload, npdDpk)
    fromObject = fmap (uncurry5 C.NewPeriodData) . fromObject

instance MessagePack C.LBlock where
    toObject C.LBlock{..} =
        toObject (lbHash, lbTransactions, lbSignature, lbHeads)
    fromObject = fmap (uncurry4 C.LBlock) . fromObject

instance MessagePack C.Transaction where
    toObject C.Transaction{..} = toObject (txInputs, txOutputs)
    fromObject = fmap (uncurry2 C.Transaction) . fromObject

instance MessagePack C.CheckConfirmation where
    toObject C.CheckConfirmation{..} =
        toObject (ccMintetteKey, ccMintetteSignature, ccHead)
    fromObject = fmap (uncurry3 C.CheckConfirmation) . fromObject

instance MessagePack C.HBlock where
    toObject C.HBlock {..} =
        toObject (hbHash, hbTransactions, hbSignature, hbDpk)
    fromObject = fmap (uncurry4 C.HBlock) . fromObject

toObj
    :: MessagePack a
    => (Int, a) -> Object
toObj = toObject

instance MessagePack C.ActionLogEntry where
    toObject (C.QueryEntry tx) = toObj (0, tx)
    toObject (C.CommitEntry tx cc) = toObj (1, (tx, cc))
    toObject (C.CloseEpochEntry heads) = toObj (2, heads)
    fromObject obj = do
        (i,payload) <- fromObject obj
        case (i :: Int) of
            0 -> C.QueryEntry <$> fromObject payload
            1 -> uncurry2 C.CommitEntry <$> fromObject payload
            2 -> C.CloseEpochEntry <$> fromObject payload
            _ -> Nothing
