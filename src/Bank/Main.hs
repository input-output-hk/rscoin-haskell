{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Text       as T
import           Data.Time.Units (Second)

import qualified BankOptions     as Opts
import qualified RSCoin.Bank     as B
import           RSCoin.Core     (Explorer (..), Mintette (Mintette),
                                  constructPublicKey, initLogging, logWarning,
                                  readPublicKey, readSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    bankSecretKey <- readSecretKey cloSkPath
    let confPath = Just cloConfigPath
    case cloCommand of
        Opts.AddMintette host port pk -> do
            let m = Mintette host port
            k <- readPk pk
            B.addMintetteReq confPath bankSecretKey m k
        Opts.AddExplorer name port pk pId -> do
            k <- readPk pk
            let e =
                    Explorer
                    { explorerHost = name
                    , explorerPort = port
                    , explorerKey = k
                    }
            B.addExplorerReq confPath bankSecretKey e pId
        Opts.RemoveMintette host port ->
            B.removeExplorerReq confPath bankSecretKey host port
        Opts.RemoveExplorer host port ->
            B.removeExplorerReq confPath bankSecretKey host port
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
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
