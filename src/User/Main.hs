import qualified AcidState         as A
import qualified UserOptions       as O

import           Control.Exception (bracket)

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    bracket (A.openState walletPath 10) A.closeState $
        \_ ->
             putStrLn $
             "Program called with the following options: \n" ++ show opts
