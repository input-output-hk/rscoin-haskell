{-# LANGUAGE ScopedTypeVariables #-}

-- | Explorer Server.

module RSCoin.Explorer.Server
       ( serve
       ) where

import           Control.Exception         (throwIO)
import           Control.Lens              (view)
import           Control.Monad             (unless)
import           Control.Monad.Catch       (catch)
import           Control.Monad.Trans       (MonadIO (liftIO), lift)
import qualified Data.Text                 as T
import           Formatting                (build, int, sformat, (%))

import           Serokell.Util.Text        (listBuilderJSONIndent, show')

import           Control.TimeWarp.Rpc      (ServerT, serverTypeRestriction1)

import qualified RSCoin.Core               as C

import           RSCoin.Explorer.AcidState (AddHBlock (..),
                                            GetExpectedPeriodId (..),
                                            GetTx (..), State, query, tidyState,
                                            update)
import           RSCoin.Explorer.Channel   (Channel, ChannelItem (..),
                                            writeChannel)
import           RSCoin.Explorer.Error     (ExplorerError (EEInvalidBankSignature))

serve
    :: C.WorkMode m
    => Int -> Channel -> State -> C.SecretKey -> m ()
serve port ch st _ = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    C.serve
        port
        [ C.method (C.RSCExplorer C.EMNewBlock) $
          idr1 $ handleNewHBlock ch st
        , C.method (C.RSCExplorer C.EMGetTransaction) $
          idr2 $ handleGetTransaction st]

type ServerTE m a = ServerT m (Either T.Text a)

toServer :: C.WorkMode m => m a -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: ExplorerError) = do
        C.logWarning $ show' e
        return $ Left $ show' e

handleGetTransaction
    :: C.WorkMode m
    => State -> C.TransactionId -> ServerTE m (Maybe C.Transaction)
handleGetTransaction st tId =
    toServer $
    do tx <- query st (GetTx tId)
       let msg =
               sformat
                   ("Getting transaction with id " % build % ": " % build)
                   tId
                   tx
       tx <$ C.logDebug msg

type HBlockSigned = C.WithSignature (C.PeriodId, C.WithMetadata C.HBlock C.HBlockMetadata)

handleNewHBlock
    :: C.WorkMode m
    => Channel
    -> State
    -> HBlockSigned
    -> ServerTE m C.PeriodId
handleNewHBlock ch st signed@(C.WithSignature (newBlockId, blkWithMeta@C.WithMetadata {..}) _) =
    toServer $
    do C.logInfo $ sformat ("Received new block #" % int) newBlockId
       let newBlock = wmValue
           ret p = do
               C.logDebug $ sformat ("Now expected block is #" % int) p
               return p
           upd = do
               C.logDebug $ sformat ("HBlock hash: " % build) (C.hash newBlock)
               C.logDebug $ sformat ("HBlock metadata: " % build) wmMetadata
               C.logDebug $
                   sformat ("Transaction hashes: " % build) $
                   listBuilderJSONIndent 2 $
                   map (C.hash :: C.Transaction -> C.Hash C.Transaction) $
                   C.hbTransactions newBlock
               C.logDebug $
                   sformat ("Transactions: " % build) $
                   listBuilderJSONIndent 2 $ C.hbTransactions newBlock
               update st (AddHBlock newBlockId blkWithMeta)
               writeChannel
                   ch
                   ChannelItem {ciTransactions = C.hbTransactions newBlock}
               tidyState st
               ret (newBlockId + 1)
       bankPublicKey <- view C.bankPublicKey <$> C.getNodeContext
       unless (C.verifyWithSignature bankPublicKey signed) $
           liftIO $ throwIO EEInvalidBankSignature
       expectedPid <- query st GetExpectedPeriodId
       if expectedPid == newBlockId
           then upd
           else ret expectedPid
