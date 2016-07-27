{-# LANGUAGE TemplateHaskell #-}

-- | Aeson instances.

module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson             (ToJSON, toJSON)
import           Data.Aeson.TH          (deriveJSON, deriveToJSON)
import           Data.Aeson.Types       (Value (..))
import qualified Data.Text              as T
import           Formatting             (fixed, sformat)

import           Serokell.Aeson.Options (defaultOptionsPS)

import           RSCoin.Core.Primitives (Address, Coin, CoinAmount (..), Color,
                                         Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress)

showFPrec :: Int -> Double -> T.Text
showFPrec prec = T.dropWhileEnd (== '0') . sformat (fixed prec)

instance ToJSON CoinAmount where
    toJSON = String . showFPrec 5 . realToFrac . getAmount

$(deriveToJSON defaultOptionsPS ''Transaction)
$(deriveToJSON defaultOptionsPS ''Color)
$(deriveToJSON defaultOptionsPS ''Coin)

$(deriveJSON defaultOptionsPS ''Address)
$(deriveJSON defaultOptionsPS ''AllocationAddress)
$(deriveJSON defaultOptionsPS ''AllocationStrategy)
$(deriveJSON defaultOptionsPS ''PartyAddress)
