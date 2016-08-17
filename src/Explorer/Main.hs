{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (throwM, try)
import           Data.Monoid         ((<>))

import           ExplorerOptions     (Options (..), getOptions)
import           RSCoin.Core         (keyGen, readSecretKey, writePublicKey,
                                      writeSecretKey)
import qualified RSCoin.Core         as C
import qualified RSCoin.Explorer     as E

main :: IO ()
main = do
    Options{..} <- getOptions
    C.initLogging cloLogSeverity
    skEither <- try $ readSecretKey cloSecretKeyPath
    sk <- case skEither of
        Left (_::SomeException) | cloAutoCreateKey -> do
            putStrLn $ "Generating and putting secret keys into: " ++
                       cloSecretKeyPath
            let fpSecret = cloSecretKeyPath
            let fpPublic = cloSecretKeyPath <> ".pub"
            (sk,pk) <- keyGen
            writePublicKey fpPublic pk
            writeSecretKey fpSecret sk
            putStrLn "Wrote a keypar on the disk"
            return sk
        Left err -> throwM err
        Right sk -> return sk
    E.launchExplorerReal
        cloPortRpc
        cloPortWeb
        cloLogSeverity
        cloPath
        (E.CACustomLocation cloConfigPath)
        sk
