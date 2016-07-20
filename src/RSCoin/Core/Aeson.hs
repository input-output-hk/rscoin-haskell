{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Aeson instances.

module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson             (ToJSON, object, toJSON, (.=),
                                         FromJSON, parseJSON, (.:))
import           Data.Aeson.Types       (Value (..))
import           Data.Aeson.TH          (deriveJSON, deriveToJSON)
import qualified Data.Text              as T
import           Formatting             (fixed, sformat)

import           Serokell.Aeson.Options (defaultOptionsPS)

import           RSCoin.Core.Primitives (Address, Coin (..), Transaction)
import           RSCoin.Core.Strategy   (AllocationAddress, AllocationStrategy,
                                         PartyAddress)

showFPrec :: Int -> Double -> T.Text
showFPrec prec = T.dropWhileEnd (== '0') . sformat (fixed prec)

instance ToJSON Coin where
    toJSON Coin{..} =
        let prec = 5
        in object
               [ "getColor" .= getColor
               , "getCoin" .= showFPrec prec (realToFrac getCoin)]

instance FromJSON Coin where
    parseJSON (Object v) = Coin <$>
                           v .: "getColor" <*>
                           v .: "getCoin"
    parseJSON _ = error "Error parsing coin JSON"
    

$(deriveToJSON defaultOptionsPS ''Transaction)

$(deriveJSON defaultOptionsPS ''Address)
$(deriveJSON defaultOptionsPS ''AllocationAddress)
$(deriveJSON defaultOptionsPS ''AllocationStrategy)
$(deriveJSON defaultOptionsPS ''PartyAddress)
