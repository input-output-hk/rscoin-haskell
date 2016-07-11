
import           RSCoin.Core.Primitives     (Address)

import           Data.Proxy                 (Proxy (..))
import           Language.PureScript.Bridge (writePSTypes, buildBridge,
                                             defaultBridge, mkSumType)

main :: IO ()
main =
    -- FIXME: https://gitlab.serokell.io/rscoin/rscoin/commit/fc8d36123dba122a4ce41053e1881ea5e1873030#note_1193
    writePSTypes "block-explorer/websocket-example/src" (buildBridge defaultBridge)
        [ mkSumType (Proxy :: Proxy Address)
        ]
