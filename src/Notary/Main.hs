module Main where

import           Control.Monad.Catch (bracket)
import           Control.Monad.Trans (liftIO)
import           RSCoin.Core         (PlatformLayout (..), defaultLayout',
                                      initLogging)
import qualified RSCoin.Notary       as N
import           RSCoin.Timed        (runRealMode)

import qualified NotaryOptions       as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    let open = if cliMemMode
               then N.openMemState
               else N.openState cliPath
    let layout = defaultLayout' cliBankHost
    runRealMode layout $
        bracket (liftIO open) (liftIO . N.closeState) (N.serve $ snd $ getNotaryAddr layout)
