-- | GHC stage restriction.

module Bench.RSCoin.StageRestriction
       ( defaultOptions
       ) where

import qualified Data.Aeson.TH as A
import           Data.Char     (isPunctuation, isUpper, toLower)

headToLower :: String -> String
headToLower [] = undefined
headToLower (x:xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

defaultOptions :: A.Options
defaultOptions =
    A.defaultOptions
    { A.fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
    }
