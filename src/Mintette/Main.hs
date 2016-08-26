{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException)
import           Control.Monad.Catch (throwM, try)
import           Data.Monoid         ((<>))
import           Data.Time.Units     (Second)

import           RSCoin.Core         (initLogging, keyGen, readSecretKey,
                                      writePublicKey, writeSecretKey)
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
        Opts.DumpStatistics  -> mainDumpStatistics ctxArg opts

mainServe :: M.ContextArgument -> Opts.ServeOptions -> Opts.Options -> IO ()
mainServe ctxArg Opts.ServeOptions{..} Opts.Options{..} = do
    skEither <- try $ readSecretKey cloSecretKeyPath
    sk <-
        case skEither of
            Left (_ :: SomeException)
              | cloAutoCreateKey -> do
                  putStrLn $
                      "Generating and putting secret keys into: " ++
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
    let dbPath =
            if cloMemMode
                then Nothing
                else Just cloPath
        epochDelta = fromInteger cloEpochDelta :: Second
    M.launchMintetteReal cloRebuildDB epochDelta cloPort sk dbPath ctxArg

mainDumpStatistics :: M.ContextArgument -> Opts.Options -> IO ()
mainDumpStatistics ctxArg Opts.Options {..} = do
    M.dumpStorageStatistics cloRebuildDB cloPath ctxArg
