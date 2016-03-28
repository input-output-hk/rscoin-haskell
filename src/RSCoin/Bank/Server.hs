-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Acid.Advanced     (query')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetMintettes (..),
                                         GetPeriodId (..), State)

import           RSCoin.Core            (BankReq (..), BankRes (..), HBlock,
                                         Mintettes, PeriodId, bankPort)
import qualified RSCoin.Core            as C (serve)

serve :: State -> IO ()
serve = C.serve bankPort . handler

handler :: State -> (BankReq -> IO BankRes)
handler st ReqGetMintettes = ResGetMintettes <$> serveGetMintettes st
handler st ReqGetBlockchainHeight = ResGetBlockchainHeight <$> serveGetHeight st
handler st (ReqGetHBlock pid) = ResGetHBlock <$> serveGetHBlock st pid

serveGetMintettes :: MonadIO m => State -> m Mintettes
serveGetMintettes st = query' st GetMintettes

serveGetHeight :: MonadIO m => State -> m Int
serveGetHeight st = query' st GetPeriodId

serveGetHBlock :: MonadIO m => State -> PeriodId -> m (Maybe HBlock)
serveGetHBlock st pId = query' st $ GetHBlock pId
