{-# LANGUAGE ScopedTypeVariables #-}
-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Exception      (catch, throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid.Advanced     (query')

import           Serokell.Util.Text     (format', formatSingle', show')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetHBlocks (..),
                                         GetLogs (..), GetMintettes (..),
                                         GetPeriodId (..), State)
import           RSCoin.Bank.Error      (BankError)

import           RSCoin.Core            (ActionLog, HBlock, MintetteId,
                                         Mintettes, PeriodId, bankPort,
                                         logDebug, logError)
import qualified RSCoin.Core.Protocol   as C
import qualified RSCoin.Test            as T

serve :: T.WorkMode m => State -> m ()
serve st = do
    idr1 <- T.serverTypeRestriction0
    idr2 <- T.serverTypeRestriction0
    idr3 <- T.serverTypeRestriction1
    idr4 <- T.serverTypeRestriction2
    idr5 <- T.serverTypeRestriction3
    C.serve bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ idr1 $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ idr2 $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlock) $ idr3 $ serveGetHBlock st
        , C.method (C.RSCDump C.GetHBlocks) $ idr4 $ serveGetHBlocks st
        , C.method (C.RSCDump C.GetHBlocks) $ idr5 $ serveGetLogs st
        ]

toServer :: T.WorkMode m => IO a -> T.ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: BankError) = do
        logError $ show' e
        throwIO e

serveGetMintettes :: T.WorkMode m => State -> T.ServerT m Mintettes
serveGetMintettes st =
    toServer $
    do mts <- query' st GetMintettes
       logDebug $ formatSingle' "Getting list of mintettes: {}" mts
       return mts

serveGetHeight :: T.WorkMode m => State -> T.ServerT m PeriodId
serveGetHeight st =
    toServer $
    do pId <- query' st GetPeriodId
       logDebug $ formatSingle' "Getting blockchain height: {}" pId
       return pId

serveGetHBlock :: T.WorkMode m 
               => State -> PeriodId -> T.ServerT m (Maybe HBlock)
serveGetHBlock st pId =
    toServer $
    do mBlock <- query' st (GetHBlock pId)
       logDebug $
           format' "Getting higher-level block with periodId {}: {}" (pId, mBlock)
       return mBlock

-- Dumping Bank state

serveGetHBlocks :: T.WorkMode m 
                => State -> PeriodId -> PeriodId -> T.ServerT m [HBlock]
serveGetHBlocks st from to =
    toServer $
    do blocks <- query' st $ GetHBlocks from to
       logDebug $
           format' "Getting higher-level blocks between {} and {}"
           (from, to)
       return blocks

serveGetLogs :: T.WorkMode m 
             => State -> MintetteId -> Int -> Int -> T.ServerT m (Maybe ActionLog)
serveGetLogs st m from to =
    toServer $
    do mLogs <- query' st (GetLogs m from to)
       logDebug $
           format' "Getting action logs of mintette {} with range of entries {} to {}: {}" (m, from, to, mLogs)
       return mLogs
