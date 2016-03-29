-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Acid.Advanced     (query')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetMintettes (..),
                                         GetPeriodId (..), State)

import           RSCoin.Core            (RSCoinMethod (..), BankMethod (..), HBlock,
                                         Mintettes, PeriodId, bankPort)
import qualified RSCoin.Core            as C (serve, method)

serve :: State -> IO ()
serve st =
    C.serve bankPort
        [ C.method (RSCBank GetMintettes) $ serveGetMintettes st
        , C.method (RSCBank GetBlockchainHeight) $ serveGetHeight st
        , C.method (RSCBank GetHBlock) $ serveGetHBlock st
        ]

serveGetMintettes :: MonadIO m => State -> m Mintettes
serveGetMintettes st = query' st GetMintettes

serveGetHeight :: MonadIO m => State -> m Int
serveGetHeight st = query' st GetPeriodId

serveGetHBlock :: MonadIO m => State -> PeriodId -> m (Maybe HBlock)
serveGetHBlock st pId = query' st $ GetHBlock pId
