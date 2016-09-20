{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (throwM, try)
import           Data.Monoid         ((<>))
import           Data.Functor        (void)
import           Data.Time.Units     (Second)
import           System.Directory    (doesFileExist)

import           RSCoin.Core         (initLogging, keyGen, readSecretKey,
                                      writePublicKey, writeSecretKey,
                                      defaultSecretKeyPath, SecretKey,
                                      readSecretKey)
import           RSCoin.Core.Types   as T
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
        Opts.AddToBank addToBankOpts -> mainAddToBank ctxArg addToBankOpts opts

mainServe :: M.ContextArgument -> Opts.ServeOptions -> Opts.Options -> IO ()
mainServe ctxArg Opts.ServeOptions {..} Opts.Options {..} = do
    skEither <- try $ readSecretKey cloSecretKeyPath
    sk <-
        case skEither of
            Left (_ :: SomeException) | cloAutoCreateKey -> createKeypair cloSecretKeyPath
            Left err -> throwM err
            Right sk -> return sk
    let dbPath =
            if cloMemMode
                then Nothing
                else Just cloPath
        epochDelta = fromInteger cloEpochDelta :: Second
        env = M.mkRuntimeEnv cloActionLogsLimit sk
    M.launchMintetteReal cloRebuildDB epochDelta cloPort env dbPath ctxArg

mainAddToBank :: M.ContextArgument -> Opts.AddToBankOptions -> Opts.Options -> IO ()
mainAddToBank ctxArg Opts.AddToBankOptions {..} Opts.Options {..} = do
    skEither <- try $ readSecretKey atboSecretKeyPath
    sk <-
        case skEither of
            Left (err :: SomeException) -> throwM err
            Left err -> throwM err
            Right sk -> return sk
    M.addToBank ctxArg sk $ T.Mintette atboMintetteHost atboMintettePort

mainDumpStatistics :: M.ContextArgument -> Opts.Options -> IO ()
mainDumpStatistics ctxArg Opts.Options {..} = do
    M.dumpStorageStatistics cloRebuildDB cloPath ctxArg

mainCreatePermissionKeypair :: M.ContextArgument -> Opts.Options -> IO ()
mainCreatePermissionKeypair _ Opts.Options {..} = do
    directory <- defaultSecretKeyPath
    void $ createKeypair directory

-- TODO: should this go to RSCoin.Core.Crypto.Signing?
createKeypair :: FilePath -> IO SecretKey
createKeypair directory = do
    let fpSecret = directory
    let fpPublic = directory <> ".pub"
    putStrLn $ "Generating secret key at " ++ fpSecret
    putStrLn $ "Generating public key at " ++ fpPublic
    secretKeyExists <- doesFileExist fpSecret
    if secretKeyExists
       then do
           putStrLn "Secret key already exists, not overwriting."
           readSecretKey fpSecret
       else do
           (sk, pk) <- keyGen
           writePublicKey fpPublic pk
           writeSecretKey fpSecret sk
           putStrLn "Done."
           return sk
