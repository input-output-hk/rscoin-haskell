{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Exception      (bracket, catch, throwIO, try)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acid.Advanced     (query')
import           Data.Monoid            ((<>))
import           Data.Text              (Text, unpack)

import           Serokell.Util.Text     (format', formatSingle',
                                         listBuilderJSONIndent, show')

import           RSCoin.Bank.AcidState  (GetHBlock (..), GetMintettes (..),
                                         GetPeriodId (..), State)
import           RSCoin.Bank.Error      (BankError)

import           RSCoin.Core            (HBlock, Mintettes, PeriodId, bankPort,
                                         logError, logDebug, logWarning, logInfo)
import qualified RSCoin.Core.Protocol   as C

serve :: State -> IO ()
serve st =
    C.serve bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlock) $ serveGetHBlock st
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
       logDebug $ formatSingle' ("Getting list of mintettes: {}") mts
       return mts

serveGetHeight :: State -> C.Server PeriodId
serveGetHeight st =
    toServer $
    do pId <- query' st GetPeriodId
       logDebug $ formatSingle' ("Getting blockchain height: {}") pId
       return pId

serveGetHBlock :: State -> PeriodId -> C.Server (Either String HBlock)
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
        return . Left $ unpack e
    onJust block = do
        logDebug $ formatSingle' "High-level block: {}" block
        return $ Right block
