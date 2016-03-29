import           Actions               (proceedCommand)
import qualified RSCoin.User.AcidState as A
import qualified UserOptions           as O

import           Control.Exception     (bracket)

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    let ifBankMode = if isBankMode then Just bankModePath else Nothing
    bracket (A.openState walletPath addressesNum ifBankMode) A.closeState $
        \st ->
             do putStrLn $ "Called with options: " ++ show opts
                proceedCommand st userCommand
