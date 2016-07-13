module PSTypes
       (psPublicKey) where


import           Control.Monad.Reader.Class          (MonadReader)

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPublicKey :: PSType
psPublicKey = TypeInfo "" "MyCustomModule" "PublicKey" []
