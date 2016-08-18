module Main where

import           Control.Monad (when)
import           Data.Maybe    (mapMaybe)

import           RSCoin.Core   (constructPublicKey, initLogging, logWarning)
import qualified RSCoin.Notary as N

import qualified NotaryOptions as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    let dbPath = if cliMemMode
                 then Nothing
                 else Just cliPath
    let trustedKeys = mapMaybe constructPublicKey cliTrustedKeys
    when (length trustedKeys < length cliTrustedKeys) $
        logWarning "Not all keys were parsed!"

    N.launchNotaryReal
        cliLogSeverity
        dbPath
        (N.CACustomLocation cliConfigPath)
        cliWebPort
        trustedKeys
