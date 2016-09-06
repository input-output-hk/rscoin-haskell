module Main
       ( main
       ) where

import           Data.Proxy                                (Proxy (..))
import           Language.PureScript.Bridge                (BridgePart,
                                                            buildBridge,
                                                            defaultBridge,
                                                            mkSumType, typeName,
                                                            writePSTypes, (<|>),
                                                            (^==))

import           Language.PureScript.Bridge.PSTypes        (psInt)
import           Language.PureScript.Bridge.TypeParameters (A, B)
import qualified RSCoin.Core.Primitives                    as Prim
import qualified RSCoin.Core.Types                         as CT
import qualified RSCoin.Explorer.WebTypes                  as EWT

import           PSTypes                                   (psCoinAmount,
                                                            psHash, psIntMap,
                                                            psPosixTime,
                                                            psPublicKey)

main :: IO ()
main =
    -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
    writePSTypes
        "block-explorer/src"
        (buildBridge customBridge)
        [ mkSumType (Proxy :: Proxy EWT.ServerError)
        , mkSumType (Proxy :: Proxy EWT.ControlMsg)
        , mkSumType (Proxy :: Proxy EWT.AddressInfoMsg)
        , mkSumType (Proxy :: Proxy EWT.HBlockInfoMsg)
        , mkSumType (Proxy :: Proxy EWT.IncomingMsg)
        , mkSumType (Proxy :: Proxy EWT.OutcomingMsg)

        , mkSumType (Proxy :: Proxy EWT.CoinsMapExtension)
        , mkSumType (Proxy :: Proxy EWT.TransactionExtension)
        , mkSumType (Proxy :: Proxy EWT.HBlockExtension)

        , mkSumType (Proxy :: Proxy (CT.WithMetadata A B))

        , mkSumType (Proxy :: Proxy Prim.Transaction)
        , mkSumType (Proxy :: Proxy Prim.Color)
        , mkSumType (Proxy :: Proxy Prim.Coin)
        , mkSumType (Proxy :: Proxy Prim.Address)]
  where
    customBridge =
        defaultBridge <|> publicKeyBridge <|> wordBridge <|> hashBridge <|>
        coinAmountBridge <|> intMapBridge <|> posixTimeBridge

publicKeyBridge :: BridgePart
publicKeyBridge = typeName ^== "PublicKey" >> pure psPublicKey

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> pure psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> pure psHash

coinAmountBridge :: BridgePart
coinAmountBridge = typeName ^== "CoinAmount" >> return psCoinAmount

intMapBridge :: BridgePart
intMapBridge = typeName ^== "IntMap" >> psIntMap

posixTimeBridge :: BridgePart
posixTimeBridge = typeName ^== "NominalDiffTime" >> pure psPosixTime
