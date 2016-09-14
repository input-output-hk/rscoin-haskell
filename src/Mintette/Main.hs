{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (throwM, try)
import           Data.Monoid         ((<>))
import           Data.Time.Units     (Second)

import           RSCoin.Core         (initLogging, keyGen, readSecretKey,
                                      writePublicKey, writeSecretKey,
                                      defaultSecretKeyPath, SecretKey)
import qualified RSCoin.Mintette     as M

import qualified MintetteOptions     as Opts

main :: IO ()
main = do
    opts@Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    let ctxArg =
            if cloDefaultContext
                then M.CADefault
                else M.CACustomLocation cloConfigPath
    case cloCommand of
        Opts.Serve serveOpts -> mainServe ctxArg serveOpts opts
        Opts.DumpStatistics -> mainDumpStatistics ctxArg opts
        Opts.CreatePermissionKeypair -> mainCreatePermissionKeypair ctxArg opts

mainServe :: M.ContextArgument -> Opts.ServeOptions -> Opts.Options -> IO ()
mainServe ctxArg Opts.ServeOptions {..} Opts.Options {..} = do
    skEither <- try $ readSecretKey cloSecretKeyPath
    sk <-
        case skEither of
            Left (_ :: SomeException) | cloAutoCreateKey -> getOrCreateKeypair cloSecretKeyPath
            Left err -> throwM err
            Right sk -> return sk
    let dbPath =
            if cloMemMode
                then Nothing
                else Just cloPath
        epochDelta = fromInteger cloEpochDelta :: Second
        env = M.mkRuntimeEnv cloActionLogsLimit sk
    M.launchMintetteReal cloRebuildDB epochDelta cloPort env dbPath ctxArg

mainDumpStatistics :: M.ContextArgument -> Opts.Options -> IO ()
mainDumpStatistics ctxArg Opts.Options {..} = do
    M.dumpStorageStatistics cloRebuildDB cloPath ctxArg

mainCreatePermissionKeypair :: M.ContextArgument -> Opts.Options -> IO ()
mainCreatePermissionKeypair ctxArg Opts.Options {..} = do
    directory <- defaultSecretKeyPath
    _ <- getOrCreateKeypair directory
    return ()

-- TODO: should this go to RSCoin.Core.Crypto.Signing?
getOrCreateKeypair :: FilePath -> IO SecretKey
getOrCreateKeypair directory = do
    let fpSecret = directory
    let fpPublic = directory <> ".pub"
    putStrLn $ "Generating secret key at " ++ fpSecret
    putStrLn $ "Generating public key at " ++ fpPublic
    (sk, pk) <- keyGen
    writePublicKey fpPublic pk
    writeSecretKey fpSecret sk
    putStrLn "Done."
    return sk
