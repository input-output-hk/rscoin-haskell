{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (bracket, throwM, try)
import           Control.Monad.Trans (liftIO)
import           Data.Monoid         ((<>))

import qualified MintetteOptions     as Opts
import           RSCoin.Core         (initLogging, keyGen, readSecretKey,
                                      writePublicKey, writeSecretKey)
import qualified RSCoin.Mintette     as M
import           RSCoin.Timed        (fork_, runRealModeUntrusted)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
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
    let open =
            if cloMemMode
                then M.openMemState
                else M.openState cloPath
    runRealModeUntrusted (Just cloConfigPath) $
        bracket (liftIO open) (liftIO . M.closeState) $
        \st ->
             do fork_ $ M.runWorker sk st (Just cloPath)
                M.serve cloPort st sk
