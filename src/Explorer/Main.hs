import qualified RSCoin.Core     as C
import qualified RSCoin.Explorer as E

import           ExplorerOptions (Options (..), getOptions)

main :: IO ()
main = do
    Options{..} <- getOptions
    C.initLogging cloLogSeverity
    sk <- C.readSecretKey cloSecretKeyPath
    E.launchExplorerReal
        cloBankHost
        cloPortRpc
        cloPortWeb
        cloLogSeverity
        cloPath
        sk
