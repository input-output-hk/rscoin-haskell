{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson             (FromJSON (parseJSON), ToJSON (toJSON),
                                         Value (Object), object, (.:), (.=))
import           Data.Aeson.TH          (defaultOptions, deriveJSON)
import           Data.Map               (Map, fromList, toList)

import           RSCoin.Core.Crypto     ()
import           RSCoin.Core.Primitives (Address, Coin, Transaction)
import           RSCoin.Core.Types      (ActionLogEntry, CheckConfirmation,
                                         LBlock, Mintette, NewPeriodData)

instance (FromJSON a, Ord a, FromJSON b) => FromJSON (Map a b) where
    parseJSON (Object v) =
        fromList <$>
        v .: "map"
    parseJSON _ = fail "Map should be an object"

instance (ToJSON a, ToJSON b) => ToJSON (Map a b) where
    toJSON m = object ["map" .= toList m]

$(deriveJSON defaultOptions ''NewPeriodData)
$(deriveJSON defaultOptions ''LBlock)
$(deriveJSON defaultOptions ''Coin)
$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''Transaction)
$(deriveJSON defaultOptions ''CheckConfirmation)
$(deriveJSON defaultOptions ''ActionLogEntry)
$(deriveJSON defaultOptions ''Mintette)
