{-# LANGUAGE FlexibleContexts #-}

import           Data.Text   as T

import qualified RSCoin.Bank as B
import           RSCoin.Core (Mintette (Mintette), bankLoggerName,
                              constructPublicKey, initLogging, logWarning,
                              readPublicKey, readSecretKey)

import qualified Options     as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        (Opts.AddMintette name port pk) -> do
            let m = Mintette name port
            k <-
                maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
            B.addMintetteIO cloPath m k
        (Opts.Serve skPath) -> B.launchBank cloPath =<< readSecretKey skPath
  where
    readPublicKeyFallback pk = do
        logWarning
            bankLoggerName
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
