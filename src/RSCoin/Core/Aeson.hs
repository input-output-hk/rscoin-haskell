{-# LANGUAGE TemplateHaskell #-}
module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson         (FromJSON (parseJSON), ToJSON (toJSON),
                                     (.=), (.:), object, Value (Object))
import           Data.Aeson.TH      (deriveJSON, defaultOptions)
import           Data.Map           (Map, toList, fromList)

import           RSCoin.Core.Crypto     ()
import           RSCoin.Core.Primitives (Transaction, Address, Coin)
import           RSCoin.Core.Types      (Mintette, ActionLogEntry,
                                         CheckConfirmation, LogChainHeads,
                                         LBlock)

instance (FromJSON a, Ord a, FromJSON b) => FromJSON (Map a b) where
    parseJSON (Object v) =
        fromList <$>
        v .: "map"
    parseJSON _ = fail "Map should be an object"

instance (ToJSON a, ToJSON b) => ToJSON (Map a b) where
    toJSON m = object ["map" .= toList m]
    
$(deriveJSON defaultOptions ''LBlock)
$(deriveJSON defaultOptions ''Coin)
$(deriveJSON defaultOptions ''Address)
$(deriveJSON defaultOptions ''Transaction)
$(deriveJSON defaultOptions ''CheckConfirmation)
$(deriveJSON defaultOptions ''ActionLogEntry)
$(deriveJSON defaultOptions ''Mintette)
