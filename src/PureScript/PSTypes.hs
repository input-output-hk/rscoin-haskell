{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PSTypes
       ( psPublicKey
       , psHash
       , psCoinAmount
       , psIntMap
       , psWithMetadata
       ) where

import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (..))

import           Language.PureScript.Bridge.Builder  (BridgeData,
                                                      psTypeParameters)

import           Control.Monad.Reader.Class

psPublicKey :: PSType
psPublicKey = TypeInfo "" "Data.Types" "PublicKey" []

psHash :: PSType
psHash = TypeInfo "" "Data.Types" "Hash" []

psCoinAmount :: PSType
psCoinAmount = TypeInfo "" "Data.Types" "CoinAmount, Bla" []

psIntMap :: MonadReader BridgeData m => m PSType
psIntMap = do
    -- NOTE: this is safe because we expect IntMap to be haskells IntMap which
    -- has one type paramater, thus it should typecheck
    -- TODO: add typePackage and typeModule checkers
    t <- head <$> psTypeParameters
    pure $ TypeInfo "purescript-prim" "Prim" "Array"
        [ TypeInfo "purescript-tuples" "Data.Tuple" "Tuple"
             [ TypeInfo "purescript-prim" "Prim" "Int" []
             , t
             ]
        ]

psWithMetadata :: MonadReader BridgeData m => m PSType
psWithMetadata =
    TypeInfo "" "RSCoin.Core.Types" "WithMetadata" <$> psTypeParameters
