{-# LANGUAGE ScopedTypeVariables #-}
-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Exception      (catch, throwIO)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid.Advanced     (query')
import           Data.Text              (Text)

import           Serokell.Util.Text     (format', formatSingle', show')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetHBlocks (..),
                                         GetLogs (..), GetMintettes (..),
                                         GetPeriodId (..), State)
import           RSCoin.Bank.Error      (BankError)

import           RSCoin.Core            (ActionLog, HBlock, MintetteId,
                                         Mintettes, PeriodId, bankPort,
                                         logDebug, logError, logWarning)
import qualified RSCoin.Core.Protocol   as C

serve :: State -> IO ()
serve st =
    C.serve bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlock) $ serveGetHBlock st
        , C.method (C.RSCDump C.GetHBlocks) $ serveGetHBlocks st
        , C.method (C.RSCDump C.GetHBlocks) $ serveGetLogs st
        ]

toServer :: IO a -> C.Server a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: BankError) = do
        logError $ show' e
        throwIO e

serveGetMintettes :: State -> C.Server Mintettes
serveGetMintettes st =
    toServer $
    do mts <- query' st GetMintettes
       logDebug $ formatSingle' "Getting list of mintettes: {}" mts
       return mts

serveGetHeight :: State -> C.Server PeriodId
serveGetHeight st =
    toServer $
    do pId <- query' st GetPeriodId
       logDebug $ formatSingle' "Getting blockchain height: {}" pId
       return pId

serveGetHBlock :: State -> PeriodId -> C.Server (Either Text HBlock)
serveGetHBlock st pId =
    toServer $
    do logDebug $
           formatSingle' "Getting higher-level block with periodId {}" pId
       maybe onNothing onJust =<< query' st (GetHBlock pId)
  where
    onNothing = do
        let e = formatSingle'
                    "Higher-level block with periodId {} doesn't exist"
                    pId
        logWarning e
        return $ Left e
    onJust block = do
        logDebug $ formatSingle' "High-level block: {}" block
        return $ Right block

-- Dumping Bank state

serveGetHBlocks :: State -> PeriodId -> PeriodId -> C.Server [HBlock]
serveGetHBlocks st from to =
    toServer $
    do blocks <- query' st $ GetHBlocks from to
       logDebug $
           format' "Getting higher-level blocks between {} and {}"
           (from, to)
       return blocks

serveGetLogs :: State -> MintetteId -> Int -> Int -> C.Server (Either Text ActionLog)
serveGetLogs st m from to =
    toServer $
    do logDebug $
           format' "Getting action logs of mintette {} with range of entries {} to {}" (m, from, to)
       maybe onNothing onJust =<< query' st (GetLogs m from to)
  where
    onNothing = do
        let e = formatSingle' "Action logs of mintette {} don't exists" m
        logWarning e
        return $ Left e
    onJust aLog = do
        logDebug $
            format'
                "Action logs of mintette {} (range {} - {}): {}"
                (m, from, to, aLog)
        return $ Right aLog
