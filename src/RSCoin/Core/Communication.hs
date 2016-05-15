{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
-- | This module provides high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( CommunicationError (..)
       , getBlockchainHeight
       , getBlockByHeight
       , getGenesisBlock
       , checkNotDoubleSpent
       , commitTx
       , sendPeriodFinished
       , announceNewPeriod
       , getOwnersByAddrid
       , getOwnersByTx
       , P.unCps
       , getBlocks
       , getMintettes
       , getLogs
       , getMintetteUtxo
       , getMintetteBlocks
       , getMintetteLogs
       ) where

import           Control.Exception          (Exception (..))
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.Trans        (MonadIO, liftIO)
import           Data.Maybe                 (fromJust)
import           Data.MessagePack           (MessagePack)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Text.Buildable        (Buildable (build))
import           Data.Tuple.Select          (sel1)
import           Data.Typeable              (Typeable)
import qualified Network.MessagePack.Client as MP (RpcError (..))

import           Safe                       (atMay)
import           Serokell.Util.Text         (format', formatSingle',
                                             listBuilderJSONIndent, mapBuilder,
                                             pairBuilder, show')

import           RSCoin.Core.Crypto         (Signature, hash)
import           RSCoin.Core.Error          (rscExceptionFromException,
                                             rscExceptionToException)
import qualified RSCoin.Core.Logging        as L
import           RSCoin.Core.Owners         (owners)
import           RSCoin.Core.Primitives     (AddrId, Transaction, TransactionId)
import qualified RSCoin.Core.Protocol       as P
import           RSCoin.Core.Types          (ActionLog, CheckConfirmation,
                                             CheckConfirmations,
                                             CommitConfirmation, HBlock, LBlock,
                                             Mintette, MintetteId, Mintettes,
                                             NewPeriodData, PeriodId,
                                             PeriodResult, Utxo)
import           RSCoin.Timed               (MonadTimed, MonadTimedError (..),
                                             WorkMode)

logError, logWarning, logInfo :: MonadIO m => Text -> m ()
logError = L.logError L.communicationLoggerName
logWarning = L.logWarning L.communicationLoggerName
logInfo = L.logInfo L.communicationLoggerName

-- | Errors which may happen during remote call.
data CommunicationError
    = ProtocolError Text  -- ^ Message was encoded incorrectly.
    | TimeoutError Text   -- ^ Waiting too long for the reply
    | MethodError Text    -- ^ Error occured during method execution.
    deriving (Show, Typeable)

instance Exception CommunicationError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance Buildable CommunicationError where
    build (ProtocolError t) = "internal error: " <> build t
    build (TimeoutError t) = "timeout error: " <> build t
    build (MethodError t) = "method error: " <> build t

rpcErrorHandler :: MonadIO m => MP.RpcError -> m a
rpcErrorHandler = liftIO . log' . fromError
  where
    log' (e :: CommunicationError) = do
        logError $ show' e
        throwM e
    fromError (MP.ProtocolError s) = ProtocolError $ pack s
    fromError (MP.ResultTypeError s) = ProtocolError $ pack s
    fromError (MP.ServerError obj) = MethodError $ pack $ show obj

monadTimedHandler :: (MonadTimed m, MonadIO m) => MonadTimedError -> m a
monadTimedHandler = log' . fromError
  where
    log' (e :: CommunicationError) = do
        logError $ show' e
        throwM e
    fromError (MTTimeoutError s) = TimeoutError s

handleErrors :: (WorkMode m, MessagePack a) => m a -> m a
handleErrors action = action `catch` rpcErrorHandler `catch` monadTimedHandler

callBank :: (WorkMode m, MessagePack a) => P.Client a -> m a
callBank = handleErrors . P.callBankSafe

callMintette :: (WorkMode m, MessagePack a) => Mintette -> P.Client a -> m a
callMintette m = handleErrors . P.callMintetteSafe m

withResult :: WorkMode m => IO () -> (a -> IO ()) -> m a -> m a
withResult before after action = do
    liftIO before
    a <- action
    liftIO $ after a
    return a

-- | Retrieves blockchainHeight from the server
getBlockchainHeight :: WorkMode m => m PeriodId
getBlockchainHeight =
    withResult
        (logInfo "Getting blockchain height")
        (logInfo . formatSingle' "Blockchain height is {}")
        $ callBank $ P.call (P.RSCBank P.GetBlockchainHeight)

-- TODO: should this method return Maybe HBlock ?
-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: WorkMode m => PeriodId -> m HBlock
getBlockByHeight pId =
    (fromJust <$>) . withResult
        infoMessage
        (maybe onError onSuccess)
        $ callBank $ P.call (P.RSCBank P.GetHBlock) pId
  where
    infoMessage =
        logInfo $ formatSingle' "Getting block with height {}" pId
    onError = do
        let e = formatSingle' "Getting block with height {} failed." pId
        logWarning e
        throwM $ MethodError e
    onSuccess res = do
        logInfo $
            format' "Successfully got block with height {}: {}" (pId, res)

getGenesisBlock :: WorkMode m => m HBlock
getGenesisBlock = do
    liftIO $ logInfo "Getting genesis block"
    block <- getBlockByHeight 0
    liftIO $ logInfo "Successfully got genesis block"
    return block

getOwnersByHash :: WorkMode m => TransactionId -> m [(Mintette, MintetteId)]
getOwnersByHash tId =
    withResult
        (logInfo $ formatSingle' "Getting owners by transaction id {}" tId)
        (logInfo . format' "Successfully got owners by hash {}: {}" . (tId,) . mapBuilder)
        $ toOwners <$> (callBank $ P.call $ P.RSCBank P.GetMintettes)
  where
    toOwners mts =
        map
            (\i -> (mts !! i, i)) $
        owners mts tId

-- | Gets owners from Transaction
getOwnersByTx :: WorkMode m => Transaction -> m [(Mintette, MintetteId)]
getOwnersByTx tx =
    withResult
        (logInfo $ formatSingle' "Getting owners by transaction {}" tx)
        (const $ logInfo "Successfully got owners by transaction")
        $ getOwnersByHash $ hash tx

-- | Gets owners from Addrid
getOwnersByAddrid :: WorkMode m => AddrId -> m [(Mintette, MintetteId)]
getOwnersByAddrid aId =
    withResult
        (logInfo $ formatSingle' "Getting owners by addrid {}" aId)
        (const $ logInfo "Successfully got owners by addrid")
        $ getOwnersByHash $ sel1 aId

checkNotDoubleSpent
    :: WorkMode m
    => Mintette
    -> Transaction
    -> AddrId
    -> Signature
    -> m (Maybe CheckConfirmation)
checkNotDoubleSpent m tx a s =
    withResult
        infoMessage
        (maybe onError onSuccess)
        $ callMintette m $ P.call (P.RSCMintette P.CheckTx) tx a s
  where
    infoMessage =
        logInfo $
            format' "Checking addrid ({}) from transaction: {}" (a, tx)
    onError =
        logWarning "Checking double spending failed."
    onSuccess res = do
        logInfo $
            format' "Confirmed addrid ({}) from transaction: {}" (a, tx)
        logInfo $ formatSingle' "Confirmation: {}" res

commitTx
    :: WorkMode m
    => Mintette
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> m (Maybe CommitConfirmation)
commitTx m tx pId cc = do
    withResult
        infoMessage
        (maybe onError onSuccess)
        $ callMintette m $ P.call (P.RSCMintette P.CommitTx) tx pId cc
  where
    infoMessage =
        logInfo $
            format' "Commit transaction {}, provided periodId is {}" (tx, pId)
    onError = do
        logWarning "CommitTx failed."
    onSuccess _ = do
        logInfo $
            formatSingle' "Successfully committed transaction {}" tx

sendPeriodFinished :: WorkMode m => Mintette -> PeriodId -> m PeriodResult
sendPeriodFinished mintette pId =
    withResult
        infoMessage
        successMessage
        $ callMintette mintette $ P.call (P.RSCMintette P.PeriodFinished) pId
  where
    infoMessage =
        logInfo $
            format' "Send period {} finished to mintette {}" (pId, mintette)
    successMessage (_,blks,lgs) =
        logInfo $
            format' "Received period result from mintette {}: \n Blocks: {}\n Logs: {}\n"
            (mintette, listBuilderJSONIndent 2 blks, lgs)

announceNewPeriod :: WorkMode m => Mintette -> NewPeriodData -> m ()
announceNewPeriod mintette npd = do
    logInfo $
        format' "Announce new period to mintette {}, new period data {}" (mintette, npd)
    callMintette
        mintette
        (P.call (P.RSCMintette P.AnnounceNewPeriod) npd)

-- Dumping Bank state

getBlocks :: WorkMode m => PeriodId -> PeriodId -> m [HBlock]
getBlocks from to =
    withResult
        infoMessage
        successMessage
        $ callBank $ P.call (P.RSCDump P.GetHBlocks) from to
  where
    infoMessage =
        logInfo $
            format' "Getting higher-level blocks between {} and {}"
            (from, to)
    successMessage res =
        logInfo $
            format' "Got higher-level blocks between {} {}: {}"
            (from, to, listBuilderJSONIndent 2 res)

getMintettes :: WorkMode m => m Mintettes
getMintettes =
    withResult
        (logInfo "Getting list of mintettes")
        (logInfo . formatSingle' "Successfully got list of mintettes {}")
        $ callBank $ P.call (P.RSCBank P.GetMintettes)

getLogs :: WorkMode m => MintetteId -> Int -> Int -> m (Maybe ActionLog)
getLogs m from to = do
    withResult
        infoMessage
        (maybe onError onSuccess)
        $ callBank $ P.call (P.RSCDump P.GetLogs) m from to
  where
    infoMessage =
        logInfo $
            format' "Getting action logs of mintette {} with range of entries {} to {}" (m, from, to)
    onError = do
        logWarning $
            format'
                "Action logs of mintette {} (range {} - {}) failed."
                (m, from, to)
    onSuccess aLog = do
        logInfo $
            format'
                "Action logs of mintette {} (range {} - {}): {}"
                (m, from, to, aLog)

-- Dumping Mintette state

getMintetteUtxo :: WorkMode m => MintetteId -> m Utxo
getMintetteUtxo mId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = formatSingle' "Mintette with this index {} doesn't exist" mId
        logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult
            (logInfo "Getting utxo")
            (logInfo . formatSingle' "Corrent utxo is: {}")
            (callMintette mintette $ P.call (P.RSCDump P.GetMintetteUtxo))

getMintetteBlocks :: WorkMode m => MintetteId -> PeriodId -> m (Maybe [LBlock])
getMintetteBlocks mId pId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = formatSingle' "Mintette with this index {} doesn't exist" mId
        logWarning e
        throwM $ MethodError e
    onJust mintette = do
        withResult
            infoMessage
            (maybe onError onSuccess)
            $ callMintette mintette $ P.call (P.RSCDump P.GetMintetteBlocks) pId
      where
        infoMessage =
            logInfo $
                format' "Getting blocks of mintette {} with period id {}" (mId, pId)
        onError = do
            logWarning $
                format' "Getting blocks of mintette {} with period id {} failed"
                (mId, pId)
        onSuccess res = do
            logInfo $
                format'
                    "Successfully got blocks for period id {}: {}"
                    (pId, listBuilderJSONIndent 2 res)

-- TODO: code duplication as getMintetteBlocks, refactor!
getMintetteLogs :: WorkMode m => MintetteId -> PeriodId -> m (Maybe ActionLog)
getMintetteLogs mId pId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = formatSingle' "Mintette with this index {} doesn't exist" mId
        logWarning e
        throwM $ MethodError e
    onJust mintette = do
        withResult
            infoMessage
            (maybe onError onSuccess)
            $ callMintette mintette $ P.call (P.RSCDump P.GetMintetteLogs) pId
      where
        infoMessage =
            logInfo $
                format' "Getting logs of mintette {} with period id {}" (mId, pId)
        onError = do
            logWarning $
                format' "Getting logs of mintette {} with period id {} faild" (mId, pId)
        onSuccess res = do
            logInfo $
                format'
                    "Successfully got logs for period id {}: {}"
                    (pId, listBuilderJSONIndent 2 $ map pairBuilder res)
