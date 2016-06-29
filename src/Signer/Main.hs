module Main where

import           Control.Monad.Catch (bracket)
import           Control.Monad.Trans (liftIO)
import           RSCoin.Core         (initLogging)
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
    runRealMode cliBankHost $
        bracket (liftIO open) (liftIO . S.closeState) (S.serve cliPort)