{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Aeson instances.

module RSCoin.Core.Aeson
       (
       ) where

import           Data.Aeson             (ToJSON, object, toJSON, (.=))
import           Data.Aeson.TH          (deriveToJSON)
import qualified Data.Text              as T
import           Formatting             (fixed, sformat)

import           Serokell.Aeson.Options (defaultOptionsPS)

import           RSCoin.Core.Primitives (Coin (..), Transaction)

showFPrec :: Int -> Double -> T.Text
showFPrec prec = T.dropWhileEnd (== '0') . sformat (fixed prec)

instance ToJSON Coin where
    toJSON Coin{..} =
        let prec = 5
        in object
               [ "getColor" .= getColor
               , "getValue" .= showFPrec prec (realToFrac getCoin)]

$(deriveToJSON defaultOptionsPS ''Transaction)
