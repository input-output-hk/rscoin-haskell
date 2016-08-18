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
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
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
        ctxArg =
            if cloDefaultContext
                then M.CADefault
                else M.CACustomLocation cloConfigPath
        epochDelta = fromInteger cloEpochDelta :: Second
    M.launchMintetteReal epochDelta cloPort sk dbPath ctxArg
