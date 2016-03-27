-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           RSCoin.Bank.AcidState (State)

import qualified RSCoin.Core as C
import           RSCoin.Core (Handler, BankReq (..), BankRes (..))

serve :: Int -> State -> IO ()
serve port state = C.serve port $ handler state

handler :: State -> Handler BankReq BankRes
handler state ReqGetMintettes = return . Right $ ResGetMintettes undefined
