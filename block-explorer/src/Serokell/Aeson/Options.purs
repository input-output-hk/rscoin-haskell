-- This options should mirror options from this Haskell module
-- https://gitlab.serokell.io/serokell-team/serokell-core/blob/master/src/Serokell/Aeson/Options.hs

module Serokell.Aeson.Options where

import Prelude

import Data.Argonaut.Generic.Aeson   as A
import Data.Argonaut.Generic.Options (Options (..))
import Serokell.Data.Char            (isLower, isPunctuation, isUpper, toLower)
import Data.String                   (uncons, singleton, dropWhile, toCharArray,
                                      fromCharArray)
import Data.Maybe                    (maybe', maybe)
import Data.Array                    (filter, findIndex, drop, elemLastIndex)

import Partial.Unsafe                (unsafeCrashWith)

headToLower :: String -> String
headToLower = maybe' (\ _ -> unsafeCrashWith "headToLower: undefined") (\{head:h, tail:t} -> singleton (toLower h) <> t ) <<< uncons

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not <<< isUpper)

dropPunctuation :: String -> String
dropPunctuation = fromCharArray <<< filter (not <<< isPunctuation) <<< toCharArray

-- TODO: use when argonaut adds labelFieldModifier
dropLensUnderscore :: String -> String
dropLensUnderscore = maybe' (\ _ -> unsafeCrashWith "dropLensUnderscore: undefined") (\{head:h, tail:t} -> dropUnderscore (singleton h) <> t ) <<< uncons
  where
    dropUnderscore "_" = ""
    dropUnderscore x = x

stripConstructorPrefix :: String -> String
stripConstructorPrefix t =
    fromCharArray <<< maybe ts (flip drop ts <<< decrementSafe) $ findIndex isLower ts
  where
    decrementSafe 0 = 0
    decrementSafe i = i - 1
    ts = toCharArray t

fixArgonautBug :: String -> String
fixArgonautBug t =
    fromCharArray <<< maybe ts (flip drop ts <<< succ) $ elemLastIndex '.' ts
  where
    succ n = n + 1
    ts = toCharArray t

defaultOptions :: Options
defaultOptions = case A.options of
                   Options opts -> Options $ opts { constructorTagModifier = headToLower <<< stripConstructorPrefix <<< fixArgonautBug }
