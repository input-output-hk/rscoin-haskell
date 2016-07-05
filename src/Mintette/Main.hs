import           Control.Monad.Catch (bracket)
import           Control.Monad.Trans (liftIO)
import           RSCoin.Core         (initLogging, readSecretKey, defaultLayout')
import qualified RSCoin.Mintette     as M
import           RSCoin.Timed        (fork_, runRealMode)

import qualified Options             as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    sk <- readSecretKey cloSecretKeyPath
    let open =
            if cloMemMode
                then M.openMemState
                else M.openState cloPath
    runRealMode (defaultLayout' cloBankHost) $
        bracket (liftIO open) (liftIO . M.closeState) $
        \st ->
             do fork_ $ M.runWorker sk st
                M.serve cloPort st sk
