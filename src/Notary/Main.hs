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
    let dbPath =
            if cliMemMode
                then Nothing
                else Just cliPath
    let trustedKeys = mapMaybe constructPublicKey cliTrustedKeys
    let ctxArg =
            if cloDefaultContext
                then N.CADefault
                else N.CACustomLocation cliConfigPath
    when (length trustedKeys < length cliTrustedKeys) $
        logWarning "Not all keys were parsed!"
    N.launchNotaryReal
        cliLogSeverity
        cloRebuildDB
        dbPath
        ctxArg
        cliWebPort
        trustedKeys
        (fromIntegral cliAllocAlive)
        (fromIntegral cliTxAlive)
