-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Acid.Advanced     (query')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetMintettes (..),
                                         GetPeriodId (..), State)

import           RSCoin.Core            (BankReq (..), BankRes (..), HBlock,
                                         Handler, Mintettes, PeriodId)
import qualified RSCoin.Core            as C (serve)

serve :: Int -> State -> IO ()
serve port state = C.serve port $ handler state

handler :: State -> Handler BankReq BankRes
handler _ ReqGetMintettes = return . Right $ ResGetMintettes undefined
handler _ ReqGetBlockchainHeight = undefined
handler _ (ReqGetHBlock _) = undefined

serveGetMintettes :: MonadIO m => State -> m Mintettes
serveGetMintettes st = query' st GetMintettes

serveGetHeight :: MonadIO m => State -> m Int
serveGetHeight st = query' st GetPeriodId

serveGetHBlock :: MonadIO m => State -> PeriodId -> m (Maybe HBlock)
serveGetHBlock st pId = query' st $ GetHBlock pId
