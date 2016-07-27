module PSTypes
       ( psPublicKey
       , psHash
       , psCoinAmount
       , psCoinsMap
       ) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

psPublicKey :: PSType
psPublicKey = TypeInfo "" "Data.Types" "PublicKey" []

psHash :: PSType
psHash = TypeInfo "" "Data.Types" "Hash" []

psCoinAmount :: PSType
psCoinAmount = TypeInfo "" "Data.Types" "CoinAmount" []

-- FIXME: there must be a way to do this more elegantly but currently we don't have time
-- NOTE: we can implement this using custom `Generic` or even better custom toJson/fromJson in Data.Types
psCoinsMap :: PSType
psCoinsMap =
    TypeInfo "purescript-prim" "Prim" "Array"
        [ TypeInfo "purescript-tuples" "Data.Tuple" "Tuple"
             [ TypeInfo "" "RSCoin.Core.Primitives" "Color" []
             , TypeInfo "" "RSCoin.Core.Primitives" "Coin" []
             ]
        ]
