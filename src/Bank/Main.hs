{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (throwM, try)
import           Data.Monoid         ((<>))
import qualified Data.Text           as T
import           Data.Time.Units     (Second)

import qualified BankOptions         as Opts
import qualified RSCoin.Bank         as B
import           RSCoin.Core         (Explorer (..), Mintette (Mintette),
                                      constructPublicKey, initLogging, keyGen,
                                      logWarning, readPublicKey, readSecretKey,
                                      testBankSecretKey, writePublicKey,
                                      writeSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    bankSecretKeyEither <-
        if cloDefaultContext
            then pure $ pure testBankSecretKey
            else try $ readSecretKey cloSkPath
    bankSecretKey <-
        case bankSecretKeyEither of
            Left (_ :: SomeException)
              | cloAutoCreateKey -> do
                  putStrLn $
                      "Generating and putting secret keys into: " ++ cloSkPath
                  let fpSecret = cloSkPath
                  let fpPublic = cloSkPath <> ".pub"
                  (sk,pk) <- keyGen
                  writePublicKey fpPublic pk
                  writeSecretKey fpSecret sk
                  putStrLn "Wrote a keypar on the disk"
                  return sk
            Left err -> throwM err
            Right sk -> return sk
    let ca =
            if cloDefaultContext
                then B.CADefault
                else B.CACustomLocation cloConfigPath
    case cloCommand of
        Opts.AddMintette host port pk -> do
            let m = Mintette host port
            k <- readPk pk
            B.addMintetteReq ca bankSecretKey m k
        Opts.AddExplorer name port pk pId -> do
            k <- readPk pk
            let e =
                    Explorer
                    { explorerHost = name
                    , explorerPort = port
                    , explorerKey = k
                    }
            B.addExplorerReq ca bankSecretKey e pId
        Opts.RemoveMintette host port ->
            B.removeMintetteReq ca bankSecretKey host port
        Opts.RemoveExplorer host port ->
            B.removeExplorerReq ca bankSecretKey host port
        Opts.DumpStatistics -> B.dumpStatisticsReq ca bankSecretKey
        Opts.Serve -> do
            let periodDelta = fromInteger cloPeriodDelta :: Second
            B.launchBankReal cloRebuildDB periodDelta cloPath ca bankSecretKey
  where
    readPk pk = maybe (readPublicKeyFallback pk) return (constructPublicKey pk)
    readPublicKeyFallback pk = do
        logWarning
            "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
