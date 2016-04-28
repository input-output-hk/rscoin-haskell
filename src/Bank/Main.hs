import           Control.Monad.Trans     (liftIO)
import           Data.Acid                (update)
import           Data.Text                as T

import qualified RSCoin.Bank              as B
import           RSCoin.Core              (Mintette (Mintette),
                                           constructPublicKey, initLogging,
                                           readSecretKey, readPublicKey,
                                           logWarning)
import           RSCoin.Test              (runRealMode, bracket')

import qualified Options                  as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    runRealMode $ 
        bracket' (liftIO $ B.openState cloPath) (liftIO . B.closeState) 
            $ run cloCommand
  where
    run (Opts.Serve skPath) st = do
        sk <- liftIO $ readSecretKey skPath
        B.runWorker sk st
        B.serve st
    run (Opts.AddMintette name port pk) st = liftIO $ do
        let m = Mintette name port
        k <- maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
        update st $ B.AddMintette m k
    readPublicKeyFallback pk = liftIO $ do
        logWarning "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
