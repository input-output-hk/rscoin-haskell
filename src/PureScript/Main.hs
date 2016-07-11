
import           RSCoin.Core.Primitives     (Address)

import           Data.Proxy                 (Proxy (..))
import           Language.PureScript.Bridge (writePSTypes, buildBridge,
                                             defaultBridge, mkSumType)

main :: IO ()
main =
    writePSTypes "block-explorer/websocket-example/src" (buildBridge defaultBridge)
        [ mkSumType (Proxy :: Proxy Address)
        ]
