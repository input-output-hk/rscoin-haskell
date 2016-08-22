{-# LANGUAGE TemplateHaskell #-}

-- | Aeson instances.

module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson             (FromJSON (..), ToJSON, toJSON,
                                         withText)
import           Data.Aeson.TH          (deriveJSON)
import           Data.Aeson.Types       (Value (..))
import qualified Data.Text              as T
import           Formatting             (fixed, sformat)

import           Serokell.Aeson.Options (defaultOptionsPS)

import           RSCoin.Core.Primitives (Address, Coin, CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress, TxStrategy)

showFPrec :: Int -> Double -> T.Text
showFPrec prec =
    T.dropWhileEnd (== '.') . T.dropWhileEnd (== '0') . sformat (fixed prec)

--instance ToJSON CoinAmount where
--    toJSON = String . showFPrec 5 . realToFrac . getAmount
--
--instance FromJSON CoinAmount where
--    parseJSON = withText "CoinAmount" $ pure . CoinAmount . read . T.unpack

$(deriveJSON defaultOptionsPS ''Address)
$(deriveJSON defaultOptionsPS ''AllocationAddress)
$(deriveJSON defaultOptionsPS ''AllocationStrategy)
$(deriveJSON defaultOptionsPS ''Coin)
$(deriveJSON defaultOptionsPS ''CoinAmount)
$(deriveJSON defaultOptionsPS ''Color)
$(deriveJSON defaultOptionsPS ''PartyAddress)
$(deriveJSON defaultOptionsPS ''Transaction)
$(deriveJSON defaultOptionsPS ''TxStrategy)
