import           RSCoin.Core     (initLogging, readSecretKey)
import qualified RSCoin.Mintette as M
import           RSCoin.Test     (runRealMode)

import qualified Options         as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    sk <- readSecretKey cloSecretKeyPath
    runRealMode $
        M.serve cloPort cloPath sk
