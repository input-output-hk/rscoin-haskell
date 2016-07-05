module Main where

import           Control.Monad.Catch (bracket)
import           Control.Monad.Trans (liftIO)
import           RSCoin.Core         (PlatformLayout (..), defaultLayout',
                                      initLogging)
import qualified RSCoin.Signer       as S
import           RSCoin.Timed        (runRealMode)

import qualified SignerOptions       as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    let open = if cliMemMode
               then S.openMemState
               else S.openState cliPath
    let layout = (defaultLayout' cliBankHost) { getSignerAddr = ("127.0.0.1", cliPort) }
    runRealMode layout $
        bracket (liftIO open) (liftIO . S.closeState) (S.serve cliPort)
