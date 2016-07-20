{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Text       as T
import           Data.Time.Units (Second)

import qualified Options         as Opts
import qualified RSCoin.Bank     as B
import           RSCoin.Core     (Address (Address), Explorer (..),
                                  Mintette (Mintette), bankLoggerName,
                                  constructPublicKey, defaultLayout',
                                  initLogging, keyGen, logWarning,
                                  readPublicKey, readSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    -- @TODO make Notary addr, bankPort configurable
    let layout = defaultLayout' "127.0.0.1"
    case cloCommand of
        Opts.AddAddress pk' strategy -> do
            addr <- Address <$> maybe (snd <$> keyGen) readPk pk'
            B.addAddressIO cloPath addr strategy
        Opts.AddMintette skPath host port pk -> do
            let m = Mintette host port
            sk <- readSecretKey skPath
            k <- maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
            B.addMintetteIO sk m k
        Opts.AddExplorer name port pk pId -> do
            k <-
                maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
            let e =
                    Explorer
                    { explorerHost = name
                    , explorerPort = port
                    , explorerKey = k
                    }
            B.addExplorerIO cloPath e pId
        Opts.Serve skPath -> do
            let periodDelta = fromInteger cloPeriodDelta :: Second
            B.launchBankReal layout periodDelta cloPath =<< readSecretKey skPath
  where
    readPk pk = maybe (readPublicKeyFallback pk) return (constructPublicKey pk)
    readPublicKeyFallback pk = do
        logWarning
            bankLoggerName
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
