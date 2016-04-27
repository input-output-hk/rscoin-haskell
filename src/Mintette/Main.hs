import           RSCoin.Core     (initLogging, readSecretKey)
import qualified RSCoin.Mintette as M
import           RSCoin.Test     (runRealMode, bracket')

import qualified Options         as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    sk <- readSecretKey cloSecretKeyPath
    runRealMode $
        bracket' (liftIO $ openState dbPath) (liftIO . closeState) $
            \st ->
                M.serve cloPort cloPath sk
