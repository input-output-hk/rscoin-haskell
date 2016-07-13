module PSTypes
       (psPublicKey) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPublicKey :: PSType
psPublicKey = TypeInfo "" "Data.Types" "PublicKey" []
