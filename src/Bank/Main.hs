{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

import qualified Data.Text                as T
import           Data.Time.Units          (Second)

import           Data.Maybe               (listToMaybe)
import qualified RSCoin.Bank              as B
import           RSCoin.Core              (Address (Address),
                                           Mintette (Mintette), bankLoggerName,
                                           constructPublicKey, initLogging,
                                           logWarning, readPublicKey,
                                           readSecretKey)
import           Serokell.Util.Exceptions (throwText)

import qualified Options                  as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        Opts.AddAddress pk strategyDef -> do
            addr <- Address <$> maybe (readPublicKeyFallback pk) return ( constructPublicKey pk)
            strategy <- maybe readStrategyFallback (return . fst) (listToMaybe . reads $ strategyDef)
            B.addAddressIO cloPath addr strategy
        Opts.AddMintette name port pk -> do
            let m = Mintette name port
            k <-
                maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
            B.addMintetteIO cloPath m k
        Opts.Serve skPath -> do
            let periodDelta = fromInteger cloPeriodDelta :: Second
            B.launchBankReal periodDelta cloPath =<< readSecretKey skPath
  where
    readPublicKeyFallback pk = do
        logWarning
            bankLoggerName
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
    readStrategyFallback =         throwText           "Failed to parse strategy"
