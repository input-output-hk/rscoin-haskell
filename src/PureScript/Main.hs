-- import qualified RSCoin.Core.Primitives     as P
-- import qualified RSCoin.Explorer.WebTypes   as T
import           Data.Proxy                 (Proxy (..))
import           Language.PureScript.Bridge (BridgePart, buildBridge,
                                             defaultBridge, mkSumType, typeName,
                                             writePSTypes, (<|>), (^==))
import           RSCoin.Explorer.WebTypes   (ServerError)

import           PSTypes                    (psPublicKey)

main :: IO ()
main =
    -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
    writePSTypes
        "block-explorer/websocket-example/src"
        (buildBridge customBridge)
        [ mkSumType (Proxy :: Proxy ServerError)
        -- mkSumType (Proxy :: Proxy P.Address)
        -- , mkSumType (Proxy :: Proxy P.Coin)
        -- , mkSumType (Proxy :: Proxy P.Color)
        -- , mkSumType (Proxy :: Proxy P.TransactionId)
        -- , mkSumType (Proxy :: Proxy P.AddrId)
        -- , mkSumType (Proxy :: Proxy P.Transaction)
        -- , mkSumType (Proxy :: Proxy T.ServerError)
        -- , mkSumType (Proxy :: Proxy T.IntroductoryMsg)
        -- , mkSumType (Proxy :: Proxy T.AddressInfoMsg)
        -- , mkSumType (Proxy :: Proxy T.OutcomingMsg)
        ]
  where
    customBridge = defaultBridge <|> publicKeyBridge

publicKeyBridge :: BridgePart
publicKeyBridge = typeName ^== "PublicKey" >> return psPublicKey
