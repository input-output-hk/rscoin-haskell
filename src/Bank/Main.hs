import           Control.Exception        (bracket)
import           Data.Acid                (update)
import           Data.Text                as T

import qualified RSCoin.Bank              as B
import           RSCoin.Core              (Mintette (Mintette),
                                           constructPublicKey, initLogging,
                                           readSecretKey, readPublicKey,
                                           logWarning)

import           Serokell.Util.Exceptions (throwText)

import qualified Options                  as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    bracket (B.openState cloPath) B.closeState $ run cloCommand
  where
    run (Opts.Serve skPath) st = do
        sk <- readSecretKey skPath
        B.runWorker sk st
        B.serve st
    run (Opts.AddMintette name port pk) st = do
        let m = Mintette name port
        k <- maybe (readPublicKeyFallback pk) return $ constructPublicKey pk
        update st $ B.AddMintette m k
    readPublicKeyFallback pk = do
        logWarning "Failed to parse public key, trying to interpret as a filepath to key"
        readPublicKey $ T.unpack pk
