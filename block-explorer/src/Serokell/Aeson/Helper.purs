module Serokell.Aeson.Helper
       ( encodeJson
       , decodeJson
       ) where

import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Argonaut.Core (Json)
import Data.Either (Either)
import Data.Generic (class Generic)

import Serokell.Aeson.Options (defaultOptions)

encodeJson :: forall a. (Generic a) => a -> Json
encodeJson = genericEncodeJson defaultOptions

decodeJson :: forall a. (Generic a) => Json -> Either String a
decodeJson = genericDecodeJson defaultOptions
