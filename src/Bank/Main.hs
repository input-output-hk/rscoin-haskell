import           Control.Exception (bracket)
import           Data.Acid         (update)

import qualified RSCoin.Bank       as B
import           RSCoin.Core       (Mintette (Mintette), readPublicKey)

import qualified Options           as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    bracket (B.openState cloPath) B.closeState $ run cloCommand
  where
    run (Opts.Serve port) st = do
        B.runWorker undefined st
        B.serve port st
    run (Opts.AddMintette name port keyPath) st = do
        k <- readPublicKey keyPath
        let m = Mintette name port
        update st $ B.AddMintette m k
