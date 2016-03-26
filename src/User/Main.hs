import qualified AcidState         as A
import           Actions           (proceedCommand)
import qualified UserOptions       as O

import           Control.Exception (bracket)

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    bracket (A.openState walletPath 10) A.closeState $
        \st ->
             do putStrLn $ "Called with options: " ++ show opts
                proceedCommand st userCommand
