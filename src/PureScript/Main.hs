import           Data.Proxy                         (Proxy (..))
import           Language.PureScript.Bridge         (BridgePart, buildBridge,
                                                     defaultBridge, mkSumType,
                                                     typeName, writePSTypes,
                                                     (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)
import qualified RSCoin.Core.Primitives             as Prim
import qualified RSCoin.Explorer.WebTypes           as EWT

import           PSTypes                            (psCoinAmount, psCoinsMap,
                                                     psHash, psPublicKey)

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
        , mkSumType (Proxy :: Proxy EWT.TransactionSummary)
        , mkSumType (Proxy :: Proxy EWT.CoinsMapSummary)

        , mkSumType (Proxy :: Proxy Prim.Color)
        , mkSumType (Proxy :: Proxy Prim.Coin)
        , mkSumType (Proxy :: Proxy Prim.Address)]
  where
    customBridge =
        defaultBridge <|> publicKeyBridge <|> wordBridge <|> hashBridge <|>
        coinAmountBridge <|> coinsMapBridge

publicKeyBridge :: BridgePart
publicKeyBridge = typeName ^== "PublicKey" >> return psPublicKey

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> return psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> return psHash

coinAmountBridge :: BridgePart
coinAmountBridge = typeName ^== "CoinAmount" >> return psCoinAmount

coinsMapBridge :: BridgePart
coinsMapBridge = typeName ^== "SerializableCoinsMap" >> return psCoinsMap
