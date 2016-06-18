-- | Stage restriction boys (TODO: move it to serokell-core maybe?).

module RSCoin.Core.CompileConfigHelper
       ( defaultOptions
       ) where

import qualified Data.Aeson.TH as A
import           Data.Char     (isLower, isPunctuation, isUpper, toLower)
import           Data.List     (findIndex)

headToLower :: String -> String
headToLower [] = undefined
headToLower (x:xs) = toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

dropPunctuation :: String -> String
dropPunctuation = filter (not . isPunctuation)

stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
    maybe t (flip drop t . decrementSafe) $ findIndex isLower t
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1

defaultOptions :: A.Options
defaultOptions =
    A.defaultOptions
    { A.fieldLabelModifier = headToLower . stripFieldPrefix . dropPunctuation
    , A.constructorTagModifier = headToLower . stripConstructorPrefix
    , A.sumEncoding = A.ObjectWithSingleField
    }
