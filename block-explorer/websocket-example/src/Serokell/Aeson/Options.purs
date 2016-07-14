-- This options should mirror options from this Haskell module https://gitlab.serokell.io/serokell-team/serokell-core/blob/master/src/Serokell/Aeson/Options.hs

module Serokell.Aeson.Options
        where

import Prelude

import Data.Argonaut.Generic.Aeson as A
import Serokell.Data.Char          (isLower, isPunctuation, isUpper, toLower)
import Data.List                   (findIndex, (:))
import Data.String                 (uncons, singleton)
import Data.Maybe                  (maybe')

import Partial.Unsafe              (unsafeCrashWith)

headToLower :: String -> String
headToLower = maybe' (const $ unsafeCrashWith "headToLower: undefined") (\{head:h, tail:t} -> singleton (toLower h) <> t ) <<< uncons
