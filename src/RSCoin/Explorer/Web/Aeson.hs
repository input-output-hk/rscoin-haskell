{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Types used for web communication.

module RSCoin.Explorer.Web.Aeson
       ( SerializableCoinsMap
       ) where

import           Control.Lens              ((^.))
import           Data.Aeson                (ToJSON (toEncoding, toJSON))
import           Data.Aeson.TH             (deriveToJSON)
import qualified Data.IntMap.Strict        as IS
import           GHC.Generics              (Generic)

import           Serokell.Aeson.Options    (defaultOptionsPS)

import qualified RSCoin.Core               as C
import           RSCoin.Explorer.Summaries (CoinsMapSummary, ExtendedAddrId,
                                            TransactionSummary (..),
                                            cmsCoinAmount, cmsCoinsMap)

-- | Newtype wrapper on top of CoinsMap which has ToJSON instance.
newtype SerializableCoinsMap =
    SerializableCoinsMap C.CoinsMap
    deriving (Show)

instance ToJSON SerializableCoinsMap where
    toJSON (SerializableCoinsMap m) = toJSON . IS.assocs $ m

data CoinsMapSummarySerializable = CoinsMapSummarySerializable
    { cmCoinsMap   :: SerializableCoinsMap
    , cmCoinAmount :: C.CoinAmount
    } deriving (Show, Generic)

mkCoinsMapSummarySerializable :: CoinsMapSummary -> CoinsMapSummarySerializable
mkCoinsMapSummarySerializable cms =
    CoinsMapSummarySerializable
    { cmCoinsMap = SerializableCoinsMap (cms ^. cmsCoinsMap)
    , cmCoinAmount = cms ^. cmsCoinAmount
    }

$(deriveToJSON defaultOptionsPS ''CoinsMapSummarySerializable)

instance ToJSON CoinsMapSummary where
    toJSON = toJSON . mkCoinsMapSummarySerializable
    toEncoding = toEncoding . mkCoinsMapSummarySerializable

data TransactionSummarySerializable = TransactionSummarySerializable
    { txId         :: C.TransactionId
    , txInputs     :: [ExtendedAddrId]
    , txOutputs    :: [(C.Address, C.Coin)]
    , txInputsSum  :: CoinsMapSummarySerializable
    , txOutputsSum :: CoinsMapSummarySerializable
    } deriving (Show, Generic)

mkTransactionSummarySerializable :: TransactionSummary -> TransactionSummarySerializable
mkTransactionSummarySerializable TransactionSummary{..} =
    TransactionSummarySerializable
    { txId = txsId
    , txInputs = txsInputs
    , txOutputs = txsOutputs
    , txInputsSum = mkCoinsMapSummarySerializable txsInputsSum
    , txOutputsSum = mkCoinsMapSummarySerializable txsOutputsSum
    }

$(deriveToJSON defaultOptionsPS ''TransactionSummarySerializable)

instance ToJSON TransactionSummary where
    toJSON = toJSON . mkTransactionSummarySerializable
    toEncoding = toEncoding . mkTransactionSummarySerializable
