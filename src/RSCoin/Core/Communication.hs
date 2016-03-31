{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
       ) where

import           Control.Exception          (Exception, catch, throwIO)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Text.Buildable        (Buildable (build))
import           Data.Tuple.Select          (sel1)
import           Data.Typeable              (Typeable)
import qualified Network.MessagePack.Client as MP (RpcError (..))

import           Serokell.Util.Text        (format', formatSingle', show',
                                            mapBuilder, listBuilderJSONIndent)

import           RSCoin.Core.Crypto         (Signature, hash)
import           RSCoin.Core.Owners         (owners)
import           RSCoin.Core.Primitives     (AddrId, Transaction, TransactionId)
import qualified RSCoin.Core.Protocol       as P
import           RSCoin.Core.Logging        (logInfo, logWarning, logError)
import           RSCoin.Core.Types          (CheckConfirmation,
                                             CheckConfirmations,
                                             CommitConfirmation, HBlock,
                                             Mintette, MintetteId,
                                             NewPeriodData, PeriodId,
                                             PeriodResult)

-- | Errors which may happen during remote call.
data CommunicationError
    = ProtocolError Text  -- ^ Message was encoded incorrectly.
    | MethodError Text    -- ^ Error occured during method execution.
    deriving (Show, Typeable)

instance Exception CommunicationError

instance Buildable CommunicationError where
    build (ProtocolError t) = "internal error: " <> build t
    build (MethodError t) = "method error: " <> build t

rpcErrorHandler :: MP.RpcError -> IO ()
rpcErrorHandler = log' . fromError
  where
    log' (e :: CommunicationError) = do
        logError $ show' e
        throwIO e
    fromError (MP.ProtocolError s) = ProtocolError $ pack s
    fromError (MP.ResultTypeError s) = ProtocolError $ pack s
    fromError (MP.ServerError obj) = MethodError $ pack $ show obj

execBank :: P.Client a -> P.WithResult a
execBank cl f = P.execBank cl f `catch` rpcErrorHandler

execMintette :: Mintette -> P.Client a -> P.WithResult a
execMintette m cl f = P.execMintette m cl f `catch` rpcErrorHandler

withResult :: IO () -> (a -> IO ()) -> P.WithResult a -> P.WithResult a
withResult before after action f = action (\a -> before >> f a >> after a)

-- | Retrieves blockchainHeight from the server
getBlockchainHeight :: P.WithResult PeriodId
getBlockchainHeight =
    withResult
        (logInfo "Getting blockchain height")
        (logInfo . formatSingle' "Blockchain height is {}")
        $ execBank $ P.call (P.RSCBank P.GetBlockchainHeight)

-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: PeriodId -> IO HBlock
getBlockByHeight pId = do
    logInfo $ formatSingle' "Getting block with height {}" pId
    res <- P.unCps $ execBank $ P.call (P.RSCBank P.GetHBlock) pId
    either onError onSuccess res
  where
    onError e = do
        logWarning $
            format' "Getting block with height {} failed with: {}" (pId, e)
        throwIO $ MethodError e
    onSuccess res = do
        logInfo $
            format' "Successfully got block with height {}: {}" (pId, res)
        return res

getGenesisBlock :: IO HBlock
getGenesisBlock = do
    logInfo "Getting genesis block"
    block <- getBlockByHeight 0
    logInfo "Successfully got genesis block"
    return block

getOwnersByHash :: TransactionId -> P.WithResult [(Mintette, MintetteId)]
getOwnersByHash tId =
    withResult
        (logInfo $ formatSingle' "Getting owners by transaction id {}" tId)
        (logInfo . format' "Successfully got owners by hash {}: {}" . (tId,) . mapBuilder)
        $ execBank $ toOwners <$> P.call (P.RSCBank P.GetMintettes)
  where
    toOwners mts =
        map
            (\i -> (mts !! i, i)) $
        owners mts tId

-- | Gets owners from Transaction
getOwnersByTx :: Transaction -> P.WithResult [(Mintette, MintetteId)]
getOwnersByTx tx =
    withResult
        (logInfo $ formatSingle' "Getting owners by transaction {}" tx)
        (const $ logInfo "Successfully got owners by transaction")
        $ getOwnersByHash $ hash tx

-- | Gets owners from Addrid
getOwnersByAddrid :: AddrId -> P.WithResult [(Mintette, MintetteId)]
getOwnersByAddrid aId =
    withResult
        (logInfo $ formatSingle' "Getting owners by addrid {}" aId)
        (const $ logInfo "Successfully got owners by addrid")
        $ getOwnersByHash $ sel1 aId

checkNotDoubleSpent
    :: Mintette
    -> Transaction
    -> AddrId
    -> Signature
    -> P.WithResult (Either Text CheckConfirmation)
checkNotDoubleSpent m tx a s =
    withResult
        infoMessage
        (either onError onSuccess)
        $ execMintette m $ P.call (P.RSCMintette P.CheckTx) tx a s
  where 
    infoMessage =
        logInfo $
            format' "Checking addrid ({}) from transaction: {}" (a, tx)
    onError e = do
        logWarning $
            formatSingle' "Checking double spending failed with: {}" e
    onSuccess res = do
        logInfo $
            format' "Confirmed addrid ({}) from transaction: {}" (a, tx)
        logInfo $ formatSingle' "Confirmation: {}" res

commitTx
    :: Mintette
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> IO (Maybe CommitConfirmation)
commitTx m tx pId cc = do
    logInfo $
        format' "Commit transaction {}, provided periodId is {}" (tx, pId)
    res <- P.unCps $ execMintette m $ P.call (P.RSCMintette P.CommitTx) tx pId cc
    either onError onSuccess res
  where
    onError (e :: Text) = do
        logWarning $
            formatSingle' "CommitTx failed: {}" e
        return Nothing
    onSuccess res = do
        logInfo $
            formatSingle' "Successfully committed transaction {}" tx
        return $ Just res

sendPeriodFinished :: Mintette -> PeriodId -> P.WithResult PeriodResult
sendPeriodFinished mintette pId =
    withResult
        infoMessage
        successMessage
        $ execMintette mintette $ P.call (P.RSCMintette P.PeriodFinished) pId
  where
    infoMessage =
        logInfo $
            format' "Send period {} finished to mintette {}" (pId, mintette)
    successMessage (_,blks,lgs) =
        logInfo $
            format' "Received period result from mintette {}: \n Blocks: {}\n Logs: {}\n"
            (mintette, listBuilderJSONIndent 2 blks, lgs)

announceNewPeriod :: Mintette -> NewPeriodData -> IO ()
announceNewPeriod mintette npd = do
    logInfo $
        format' "Announce new period to mintette {}, new period data {}" (mintette, npd)
    execMintette
        mintette
        (P.call (P.RSCMintette P.AnnounceNewPeriod) npd)
        return

-- Dumping Bank state

getBlocks :: PeriodId -> PeriodId -> P.WithResult [HBlock]
getBlocks from to =
    withResult
        infoMessage
        successMessage
        $ execBank $ P.call (P.RSCDump P.GetHBlocks) from to
  where
    infoMessage =
        logInfo $
            format' "Getting higher-level blocks between {} and {}"
            (from, to)
    successMessage res =
        logInfo $
            format' "Got higher-level blocks between {} {}: {}"
            (from, to, listBuilderJSONIndent 2 res)
