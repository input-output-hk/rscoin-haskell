import           Control.Exception (bracket)

import           AcidState         (closeState, openState)
import           Options           (Command (..), Options (..), getOptions)
import           Server            (serve)

main :: IO ()
main = do
    Options {..} <- getOptions
    bracket (openState cloPath) closeState $ run cloCommand
  where
    run (Serve port) st = serve port st
    run (AddMintette name port keyPath) st = undefined
