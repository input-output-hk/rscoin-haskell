import           Control.Exception (bracket)
import           Data.Acid         (update)

import           RSCoin.Core       (Mintette (Mintette), readPublicKey)

import qualified AcidState         as AS
import           Options           (Command (..), Options (..), getOptions)
import           Server            (serve)
import           Worker            (runWorker)

main :: IO ()
main = do
    Options{..} <- getOptions
    bracket (AS.openState cloPath) AS.closeState $ run cloCommand
  where
    run (Serve port) st = do
        runWorker st
        serve port st
    run (AddMintette name port keyPath) st = do
        k <- readPublicKey keyPath
        let m = Mintette name port
        update st $ AS.AddMintette m k
