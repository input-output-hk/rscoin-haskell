{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException)
import           Control.Monad       (when)
import           Control.Monad.Catch (throwM, try)
import           Data.Maybe          (mapMaybe)
import           Data.Monoid         ((<>))

import           RSCoin.Core         (constructPublicKey, initLogging, keyGen,
                                      logWarning, readSecretKey,
                                      testNotarySecretKey, writePublicKey,
                                      writeSecretKey)
import qualified RSCoin.Notary       as N

import qualified NotaryOptions       as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cliLogSeverity
    notarySecretKeyEither <-
        if cloDefaultContext
            then pure $ pure testNotarySecretKey
            else try $ readSecretKey cloSkPath
    notarySecretKey <-
        case notarySecretKeyEither of
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
        notarySecretKey
        dbPath
        ctxArg
        cliWebPort
        trustedKeys
        (fromIntegral cliAllocAlive)
        (fromIntegral cliTxAlive)
