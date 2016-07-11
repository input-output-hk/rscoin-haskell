import           Distribution.Simple        (defaultMainWithHooks, simpleUserHooks,
                                             UserHooks (postBuild))
import           System.Process             (system)

import           Control.Monad              (void)

import           RSCoin.Core
import           Language.PureScript.Bridge (writePSTypes, buildBridge,
                                             defaultBridge)


main = defaultMainWithHooks $ simpleUserHooks { postBuild = buildPureScript }

buildPureScript _ _ _ _ = do
    writePSTypes "" (buildBridge defaultBridge)
        [ mkSumType (Proxy :: Proxy IncomingMsg)
        ]
