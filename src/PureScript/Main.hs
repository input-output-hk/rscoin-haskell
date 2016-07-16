import           Data.Proxy                         (Proxy (..))
import           Language.PureScript.Bridge         (BridgePart, buildBridge,
                                                     defaultBridge, mkSumType,
                                                     typeName, writePSTypes,
                                                     (<|>), (^==))
import           Language.PureScript.Bridge.PSTypes (psInt)
import qualified RSCoin.Core.Primitives             as P
import qualified RSCoin.Explorer.WebTypes           as T

import           PSTypes                            (psCoinsMap, psHash,
                                                     psPublicKey, psRational)

main :: IO ()
main =
    -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
    writePSTypes
        "block-explorer/src"
        (buildBridge customBridge)
        [ mkSumType (Proxy :: Proxy T.ServerError)
        , mkSumType (Proxy :: Proxy T.IntroductoryMsg)
        , mkSumType (Proxy :: Proxy T.AddressInfoMsg)
        , mkSumType (Proxy :: Proxy T.OutcomingMsg)
        , mkSumType (Proxy :: Proxy P.Coin)
        , mkSumType (Proxy :: Proxy P.Transaction)
        , mkSumType (Proxy :: Proxy P.Address)]
  where
    -- , mkSumType (Proxy :: Proxy T.ServerError)
    -- , mkSumType (Proxy :: Proxy T.IntroductoryMsg)
    customBridge =
        defaultBridge <|> publicKeyBridge <|> wordBridge <|> hashBridge <|>
        rationalBridge <|> coinsMapBridge

publicKeyBridge :: BridgePart
publicKeyBridge = typeName ^== "PublicKey" >> return psPublicKey

wordBridge :: BridgePart
wordBridge = typeName ^== "Word" >> return psInt

hashBridge :: BridgePart
hashBridge = typeName ^== "Hash" >> return psHash

rationalBridge :: BridgePart
rationalBridge = typeName ^== "Ratio" >> return psRational

coinsMapBridge :: BridgePart
coinsMapBridge = typeName ^== "SerializableCoinsMap" >> return psCoinsMap
