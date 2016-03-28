-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           RSCoin.Bank.AcidState (State)

import           RSCoin.Core           (BankReq (..), BankRes (..), Handler)
import qualified RSCoin.Core           as C

serve :: Int -> State -> IO ()
serve port state = C.serve port $ handler state

handler :: State -> Handler BankReq BankRes
handler _ ReqGetMintettes = return . Right $ ResGetMintettes undefined
handler _ ReqGetBlockchainHeight = undefined
handler _ (ReqGetHBlock _) = undefined
