{-# LANGUAGE ViewPatterns #-}
-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Data.Acid.Advanced     (query')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetMintettes (..),
                                         GetPeriodId (..), State)

import           RSCoin.Core            (HBlock, Mintettes, PeriodId, bankPort)
import qualified RSCoin.Core.Protocol   as C

serve :: State -> IO ()
serve st =
    C.serve bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlock) $ serveGetHBlock st
        ]

serveGetMintettes :: State -> C.Server Mintettes
serveGetMintettes st = C.AsMessagePack <$> query' st GetMintettes

serveGetHeight :: State -> C.Server PeriodId
serveGetHeight st = C.AsMessagePack <$> query' st GetPeriodId

serveGetHBlock :: State -> PeriodId -> C.Server (Maybe HBlock)
serveGetHBlock st pId = fmap C.AsMessagePack . query' st $ GetHBlock pId
