{-# LANGUAGE DeriveGeneric #-}

import           Data.Proxy                                (Proxy (..))
import           Language.PureScript.Bridge                (BridgePart,
                                                            buildBridge,
                                                            defaultBridge,
                                                            mkSumType, typeName,
                                                            writePSTypes, (<|>),
                                                            (^==))

import           GHC.Generics                              (Generic)

import           Language.PureScript.Bridge.PSTypes        (psInt)
import           Language.PureScript.Bridge.TypeParameters (A, B)
import qualified RSCoin.Core.Primitives                    as Prim
import qualified RSCoin.Core.Types                         as CT
import qualified RSCoin.Explorer.WebTypes                  as EWT

import           PSTypes                                   (psCoinAmount,
                                                            psHash, psIntMap,
                                                            psPublicKey,
                                                            psWithMetadata)

main :: IO ()
main =
    -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
    writePSTypes
        "block-explorer/src"
        (buildBridge customBridge)
        [ mkSumType (Proxy :: Proxy EWT.ServerError)
        , mkSumType (Proxy :: Proxy EWT.ControlMsg)
        , mkSumType (Proxy :: Proxy EWT.AddressInfoMsg)
        , mkSumType (Proxy :: Proxy EWT.IncomingMsg)
        , mkSumType (Proxy :: Proxy EWT.OutcomingMsg)

        , mkSumType (Proxy :: Proxy EWT.CoinsMapExtension)
        , mkSumType (Proxy :: Proxy EWT.TransactionExtension)
        , mkSumType (Proxy :: Proxy EWT.HBlockExtension)
        , mkSumType (Proxy :: Proxy EWT.CoinsMapExtended)
        , mkSumType (Proxy :: Proxy EWT.TransactionExtended)
        , mkSumType (Proxy :: Proxy EWT.HBlockExtended)

        , mkSumType (Proxy :: Proxy (CT.WithMetadata A B))

        , mkSumType (Proxy :: Proxy Prim.Color)
        , mkSumType (Proxy :: Proxy Prim.Coin)
        , mkSumType (Proxy :: Proxy Prim.Address)]
  where
    customBridge =
        defaultBridge <|> publicKeyBridge <|> wordBridge <|> hashBridge <|>
        coinAmountBridge <|> intMapBridge -- <|> withMetadataBridge

data WithMetadata value metadata = WithMetadata
    { wmValue    :: value
    , wmMetadata :: metadata
    } deriving (Show, Eq, Generic)

publicKeyBridge :: BridgePart
publicKeyBridge = typeName ^== "PublicKey" >> return psPublicKey

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> return psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> return psHash

coinAmountBridge :: BridgePart
coinAmountBridge = typeName ^== "CoinAmount" >> return psCoinAmount

intMapBridge :: BridgePart
intMapBridge = typeName ^== "IntMap" >> psIntMap

-- TODO: this can be handled better but I can't make it work
-- https://hackage.haskell.org/package/purescript-bridge-0.6.0.1/docs/Language-PureScript-Bridge-TypeParameters.html
withMetadataBridge :: BridgePart
withMetadataBridge = typeName ^== "WithMetadata" >> psWithMetadata
