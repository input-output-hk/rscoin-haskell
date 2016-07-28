module Main where

import           RSCoin.Core   (initLogging)
import qualified RSCoin.Notary as N

import qualified NotaryOptions as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    let dbPath =
            if cliMemMode
                then Nothing
                else Just cliPath
    N.launchNotaryReal cliLogSeverity dbPath $ Just cliConfigPath
