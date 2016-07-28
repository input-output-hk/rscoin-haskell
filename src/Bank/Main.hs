{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Text       as T
import           Data.Time.Units (Second)

import qualified BankOptions     as Opts
import qualified RSCoin.Bank     as B
import           RSCoin.Core     (Explorer (..), Mintette (Mintette),
                                  bankLoggerName, constructPublicKey,
                                  initLogging, logWarning, readPublicKey,
                                  readSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    bankSecretKey <- readSecretKey cloSkPath
    case cloCommand of
        Opts.AddMintette host port pk -> do
            let m = Mintette host port
            k <- readPk pk
            B.addMintetteIO (Just cloConfigPath) bankSecretKey m k
        Opts.AddExplorer name port pk pId -> do
            k <- readPk pk
            let e =
                    Explorer
                    { explorerHost = name
                    , explorerPort = port
                    , explorerKey = k
                    }
            B.addExplorerIO (Just cloConfigPath) bankSecretKey e pId
        Opts.Serve -> do
            let periodDelta = fromInteger cloPeriodDelta :: Second
            B.launchBankReal
                periodDelta
                cloPath
                (Just cloConfigPath)
                bankSecretKey
  where
    readPk pk = maybe (readPublicKeyFallback pk) return (constructPublicKey pk)
    readPublicKeyFallback pk = do
        logWarning
            bankLoggerName
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
