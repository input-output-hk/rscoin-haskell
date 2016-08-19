{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | This module provides high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( CommunicationError (..)
       , getBlockchainHeight
       , getBlockByHeight
       , getBlocksByHeight
       , getTransactionById
       , getGenesisBlock
       , finishPeriod
       , sendBankLocalControlRequest
       , checkNotDoubleSpent
       , commitTx
       , getMintettePeriod
       , sendPeriodFinished
       , announceNewPeriod
       , announceNewPeriodsToNotary
       , getNotaryPeriod
       , allocateMultisignatureAddress
       , queryNotaryCompleteMSAddresses
       , removeNotaryCompleteMSAddresses
       , queryNotaryMyMSAllocations
       , announceNewBlock
       , P.unCps
       , getMintettes
       , getAddresses
       , getLogs
       , getMintetteUtxo
       , getMintetteBlocks
       , getMintetteLogs
       , publishTxToNotary
       , getTxSignatures
       , pollTxsFromNotary
       ) where

import           Control.Exception          (Exception (..))
import           Control.Monad.Catch        (catch, throwM)
import           Control.Monad.Trans        (MonadIO, liftIO)
import qualified Data.Map                   as M
import           Data.MessagePack           (MessagePack)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import qualified Data.Text.Buildable        as B (Buildable (build))
import           Data.Typeable              (Typeable)
import           Formatting                 (build, int, sformat, shown, stext,
                                             (%))
import qualified Network.MessagePack.Client as MP (RpcError (..))
import           Safe                       (atMay)

import           Serokell.Util.Text         (listBuilderJSONIndent, mapBuilder,
                                             pairBuilder, show')

import           RSCoin.Core.Crypto         (PublicKey, Signature)
import           RSCoin.Core.Error          (rscExceptionFromException,
                                             rscExceptionToException)
import           RSCoin.Core.Logging        (WithNamedLogger (..))
import qualified RSCoin.Core.Logging        as L
import           RSCoin.Core.Primitives     (AddrId, Address, EmissionId,
                                             Transaction, TransactionId)
import qualified RSCoin.Core.Protocol       as P
import           RSCoin.Core.Strategy       (AddressToTxStrategyMap,
                                             AllocationAddress, AllocationInfo,
                                             AllocationStrategy, MSAddress,
                                             PartyAddress, TxStrategy)
import           RSCoin.Core.Types          (ActionLog, CheckConfirmation,
                                             CheckConfirmations,
                                             CommitAcknowledgment,
                                             Explorer (..), HBlock, LBlock,
                                             Mintette, MintetteId, Mintettes,
                                             NewPeriodData, PeriodId,
                                             PeriodResult, Utxo)
import           RSCoin.Timed               (MonadTimed, MonadTimedError (..),
                                             WorkMode)

-- | Errors which may happen during remote call.
data CommunicationError
    = ProtocolError Text  -- ^ Message was encoded incorrectly.
    | TimeoutError Text   -- ^ Waiting too long for the reply
    | MethodError Text    -- ^ Error occured during method execution.
    deriving (Show, Typeable)

instance Exception CommunicationError where
    toException = rscExceptionToException
    fromException = rscExceptionFromException

instance B.Buildable CommunicationError where
    build (ProtocolError t) = "internal error: " <> B.build t
    build (TimeoutError t)  = "timeout error: " <> B.build t
    build (MethodError t)   = "method error: " <> B.build t

rpcErrorHandler :: (MonadIO m, WithNamedLogger m) => MP.RpcError -> m a
rpcErrorHandler = liftIO . log' . fromError
  where
    log' (e :: CommunicationError) = do
        L.logError $ show' e
        throwM e
    fromError (MP.ProtocolError s)   = ProtocolError $ pack s
    fromError (MP.ResultTypeError s) = ProtocolError $ pack s
    fromError (MP.ServerError obj)   = MethodError $ pack $ show obj

monadTimedHandler :: (MonadTimed m, MonadIO m, WithNamedLogger m) => MonadTimedError -> m a
monadTimedHandler = log' . fromError
  where
    log' (e :: CommunicationError) = do
        L.logError $ show' e
        throwM e
    fromError (MTTimeoutError s) = TimeoutError s

handleErrors :: (WorkMode m, MessagePack a) => m a -> m a
handleErrors action = action `catch` rpcErrorHandler `catch` monadTimedHandler

handleEither :: (WorkMode m, MessagePack a) => m (Either Text a) -> m a
handleEither action = do
    res <- action
    either
        (throwM . MethodError . sformat ("Error on caller side has ocurred: " % stext))
        return
        res

callBank :: (WorkMode m, MessagePack a) => P.Client (Either Text a) -> m a
callBank = handleEither . handleErrors . P.callBankSafe

callMintette :: (WorkMode m, MessagePack a) => Mintette -> P.Client a -> m a
callMintette m = handleErrors . P.callMintetteSafe m

callNotary :: (WorkMode m, MessagePack a) => P.Client (Either Text a) -> m a
callNotary = handleEither . handleErrors . P.callNotary

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
        (L.logDebug "Getting blockchain height")
        (L.logDebug . sformat ("Blockchain height is " % int))
        $ callBank $ P.call (P.RSCBank P.GetBlockchainHeight)

-- TODO: should this method return Maybe HBlock ?
-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: WorkMode m => PeriodId -> m HBlock
getBlockByHeight pId =
    withResult
        infoMessage
        onSuccess
        (head <$> callBank (P.call (P.RSCBank P.GetHBlocks) [pId]))
  where
    infoMessage =
        L.logDebug $ sformat ("Getting block with height " % int) pId
    onSuccess (res :: HBlock) =
        L.logDebug $
            sformat ("Successfully got block with height " % int % ": " % build)
                pId res

getBlocksByHeight :: WorkMode m => PeriodId -> PeriodId -> m [HBlock]
getBlocksByHeight from to =
    withResult
        infoMessage
        successMessage
        $ callBank $ P.call (P.RSCBank P.GetHBlocks) [from..to]
  where
    infoMessage =
        L.logDebug $
            sformat ("Getting higher-level blocks between " % int % " and " % int)
                from to
    successMessage res =
        L.logDebug $
            sformat
                ("Got higher-level blocks between " % int % " " %
                 int % ": " % build)
                from to (listBuilderJSONIndent 2 res)

getTransactionById :: WorkMode m => TransactionId -> m (Maybe Transaction)
getTransactionById tId =
    withResult
        (L.logInfo $ sformat ("Getting transaction by id " % build) tId)
        (\t -> L.logInfo $ sformat
                   ("Successfully got transaction by id " % build % ": " % build)
                   tId t)
        $ callBank $ P.call (P.RSCBank P.GetTransaction) tId

getGenesisBlock :: WorkMode m => m HBlock
getGenesisBlock = do
    liftIO $ L.logDebug "Getting genesis block"
    block <- getBlockByHeight 0
    liftIO $ L.logDebug "Successfully got genesis block"
    return block

finishPeriod :: WorkMode m => Signature -> m ()
finishPeriod currentPeriodSignature = do
    withResult
        (L.logInfo "Finishing period")
        (const $ L.logDebug "Successfully finished period") $
        callBank $ P.call (P.FinishPeriod currentPeriodSignature) currentPeriodSignature

sendBankLocalControlRequest :: WorkMode m => P.BankLocalControlRequest -> m ()
sendBankLocalControlRequest request =
    withResult
        (L.logInfo $ sformat ("Sending control request to bank: " % build) request)
        (const $ L.logDebug "Sent control request successfully") $
         callBank $ P.call (P.RSCBank P.LocalControlRequest) request

checkNotDoubleSpent
    :: WorkMode m
    => Mintette
    -> Transaction
    -> AddrId
    -> [(Address, Signature)]
    -> m (Either Text CheckConfirmation)
checkNotDoubleSpent m tx a s =
    withResult infoMessage (either onError onSuccess) $
    callMintette m $ P.call (P.RSCMintette P.CheckTx) tx a s
  where
    infoMessage =
        L.logDebug $ sformat ("Checking addrid (" % build % ") from transaction: " % build) a tx
    onError e =
        L.logError $ sformat ("Checking double spending failed: " % stext) e
    onSuccess res = do
        L.logDebug $
            sformat ("Confirmed addrid (" % build % ") from transaction: " % build) a tx
        L.logDebug $ sformat ("Confirmation: " % build) res

commitTx
    :: WorkMode m
    => Mintette
    -> Transaction
    -> CheckConfirmations
    -> m (Either Text CommitAcknowledgment)
commitTx m tx cc =
    withResult infoMessage (either onError onSuccess) $
    callMintette m $ P.call (P.RSCMintette P.CommitTx) tx cc
  where
    infoMessage = L.logInfo $ sformat ("Commit transaction " % build) tx
    onError e = L.logError $ sformat ("Commit tx failed: " % stext) e
    onSuccess _ =
        L.logInfo $ sformat ("Successfully committed transaction " % build) tx

getMintettePeriod :: WorkMode m => Mintette -> m (Maybe PeriodId)
getMintettePeriod m =
    withResult infoMessage (maybe onError onSuccess) $
    handleEither $ callMintette m $ P.call (P.RSCMintette P.GetMintettePeriod)
  where
    infoMessage = L.logInfo $
        sformat ("Getting minette period from mintette " % build) m
    onError = L.logError $ sformat
        ("getMintettePeriod failed for mintette " % build) m
    onSuccess p =
        L.logInfo $ sformat ("Successfully got the period: " % build) p

sendPeriodFinished :: WorkMode m => Mintette -> PeriodId -> m PeriodResult
sendPeriodFinished mintette pId =
    withResult infoMessage successMessage $
    handleEither $
    callMintette mintette $ P.call (P.RSCMintette P.PeriodFinished) pId

  where
    infoMessage =
        L.logInfo $
        sformat ("Send period " % int % " finished to mintette " % build)
            pId mintette
    successMessage (_,blks,lgs) =
        L.logInfo $
        sformat
            ("Received period result from mintette " % build % ": \n" %
            " Blocks: " % build % "\n" %
            " Logs: " % build % "\n")
            mintette (listBuilderJSONIndent 2 blks) lgs

announceNewPeriodsToNotary :: WorkMode m => PeriodId -> [HBlock] -> m ()
announceNewPeriodsToNotary pId' hblocks = do
    L.logInfo $
        sformat
            ("Announce new periods to Notary, hblocks " % build %
             ", latest periodId " %
             int)
            hblocks
            pId'
    callNotary $ P.call (P.RSCNotary P.AnnounceNewPeriodsToNotary) pId' hblocks

getNotaryPeriod :: WorkMode m => m PeriodId
getNotaryPeriod = do
    L.logInfo "Getting period of Notary"
    callNotary $ P.call $ P.RSCNotary P.GetNotaryPeriod

allocateMultisignatureAddress
    :: WorkMode m
    => Address
    -> PartyAddress
    -> AllocationStrategy
    -> Signature
    -> Maybe (PublicKey, Signature)
    -> m ()
allocateMultisignatureAddress msAddr partyAddr allocStrat signature mMasterCheck = do
    L.logInfo $ sformat
        ( "Allocate new ms address: " % build % "\n ,"
        % "from party address: "      % build % "\n ,"
        % "allocation strategy: "     % build % "\n ,"
        % "current party pair: "      % build % "\n ,"
        % "certificate chain: "       % build % "\n ,"
        )
        msAddr
        partyAddr
        allocStrat
        signature
        (pairBuilder <$> mMasterCheck)
    callNotary $ P.call (P.RSCNotary P.AllocateMultisig)
        msAddr partyAddr allocStrat signature mMasterCheck

queryNotaryCompleteMSAddresses :: WorkMode m => m [(Address, TxStrategy)]
queryNotaryCompleteMSAddresses = do
    L.logInfo "Querying Notary complete MS addresses"
    callNotary $ P.call $ P.RSCNotary P.QueryCompleteMS

removeNotaryCompleteMSAddresses :: WorkMode m => [Address] -> Signature -> m ()
removeNotaryCompleteMSAddresses addresses signedAddrs = do
    L.logInfo "Removing Notary complete MS addresses"
    callNotary $ P.call (P.RSCNotary P.RemoveCompleteMS) addresses signedAddrs

queryNotaryMyMSAllocations
    :: WorkMode m
    => AllocationAddress
    -> m [(MSAddress, AllocationInfo)]
queryNotaryMyMSAllocations allocAddr =
    withResult
        infoMessage
        successMessage
        $ callNotary $ P.call (P.RSCNotary P.QueryMyAllocMS) allocAddr
  where
    infoMessage = L.logInfo "Calling Notary for my MS addresses..."
    successMessage res =
        L.logDebug
        $ sformat ("Retrieving from Notary: " % build)
        $ mapBuilder res

announceNewPeriod :: WorkMode m => Mintette -> NewPeriodData -> m ()
announceNewPeriod mintette npd = do
    L.logInfo $
        sformat
            ("Announce new period to mintette " % build % ", new period data " %
             build)
            mintette
            npd
    handleEither $
        callMintette mintette $ P.call (P.RSCMintette P.AnnounceNewPeriod) npd


announceNewBlock
    :: WorkMode m
    => Explorer -> PeriodId -> (HBlock, EmissionId) -> Signature -> m PeriodId
announceNewBlock explorer pId blk signature =
    withResult infoMessage successMessage $
    P.callExplorer explorer $
    P.call (P.RSCExplorer P.EMNewBlock) pId blk signature
  where
    infoMessage =
        L.logInfo $
        sformat
            ("Announcing new (" % int % "-th) block to " % build)
            pId
            explorer
    successMessage respPeriod =
        L.logDebug $
        sformat
            ("Received periodId " % int % " from explorer " % build)
            respPeriod
            explorer

-- Dumping Bank state


getAddresses :: WorkMode m => m AddressToTxStrategyMap
getAddresses =
    withResult
        (L.logDebug "Getting list of addresses")
        (L.logDebug . sformat ("Successfully got list of addresses " % build) .
         mapBuilder . M.toList)
        $ callBank $ P.call (P.RSCBank P.GetAddresses)

getMintettes :: WorkMode m => m Mintettes
getMintettes =
    withResult
        (L.logDebug "Getting list of mintettes")
        (L.logDebug . sformat ("Successfully got list of mintettes " % build))
        $ callBank $ P.call (P.RSCBank P.GetMintettes)

getLogs :: WorkMode m => MintetteId -> Int -> Int -> m (Maybe ActionLog)
getLogs m from to =
    withResult infoMessage (maybe onError onSuccess) $
    callBank $ P.call (P.RSCDump P.GetLogs) m from to
  where
    infoMessage =
        L.logDebug $
        sformat
            ("Getting action logs of mintette " % build %
            " with range of entries " % int % " to " % int)
            m from to
    onError =
        L.logWarning $
        sformat
            ("Action logs of mintette " % build %
            " (range " % int % " - " % int % ") failed.")
            m from to
    onSuccess aLog =
        L.logDebug $
        sformat
            ("Action logs of mintette " % build %
             " (range " % int % " - " % int % "): " % build)
            m from to aLog

-- Dumping Mintette state

getMintetteUtxo :: WorkMode m => MintetteId -> m Utxo
getMintetteUtxo mId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = sformat ("Mintette with this index " % int % " doesn't exist") mId
        L.logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult
            (L.logDebug "Getting utxo")
            (L.logDebug . sformat ("Corrent utxo is: " % build))
            (handleEither $
             callMintette mintette $ P.call (P.RSCDump P.GetMintetteUtxo))

getMintetteBlocks :: WorkMode m => MintetteId -> PeriodId -> m (Maybe [LBlock])
getMintetteBlocks mId pId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = liftIO $ do
        let e = sformat ("Mintette with this index " % int % " doesn't exist") mId
        L.logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult
            infoMessage
            (maybe onError onSuccess) $
            handleEither $
            callMintette mintette $ P.call (P.RSCDump P.GetMintetteBlocks) pId
      where
        infoMessage =
            L.logDebug $
                sformat ("Getting blocks of mintette " % int %
                         " with period id " % int)
                mId pId
        onError =
            L.logWarning $
                sformat ("Getting blocks of mintette " % int %
                         " with period id " % int % " failed")
                mId pId
        onSuccess res =
            L.logDebug $
                sformat ("Successfully got blocks for period id " % int % ": " % build)
                    pId (listBuilderJSONIndent 2 res)

-- TODO: code duplication as getMintetteBlocks, refactor!
getMintetteLogs :: WorkMode m => MintetteId -> PeriodId -> m (Maybe ActionLog)
getMintetteLogs mId pId = do
    ms <- getMintettes
    maybe onNothing onJust $ ms `atMay` mId
  where
    onNothing = do
        let e = sformat ("Mintette with this index " % int % " doesn't exist") mId
        L.logWarning e
        throwM $ MethodError e
    onJust mintette =
        withResult infoMessage (maybe onError onSuccess) $
        handleEither $
        callMintette mintette $ P.call (P.RSCDump P.GetMintetteLogs) pId
      where
        infoMessage =
            L.logDebug $
            sformat ("Getting logs of mintette " % int % " with period id " % int)
            mId pId
        onError =
            L.logWarning $
            sformat
                ("Getting logs of mintette " % int %
                 " with period id " % int % " failed")
                mId pId
        onSuccess res =
            L.logDebug $
            sformat
                ("Successfully got logs for period id " % int % ": " % build)
                pId (listBuilderJSONIndent 2 $ map pairBuilder res)

-- | Send transaction with public wallet address & signature for it,
-- get list of signatures after Notary adds yours.
publishTxToNotary
    :: WorkMode m
    => Transaction              -- ^ transaction to sign
    -> Address                  -- ^ address of transaction input (individual, multisig or etc.)
    -> (Address, Signature)     -- ^ party's public address and signature
                                -- (made with it's secret key)
    -> m [(Address, Signature)] -- ^ signatures for all parties already signed the transaction
publishTxToNotary tx addr sg =
    withResult infoMessage successMessage $
    callNotary $ P.call (P.RSCNotary P.PublishTransaction) tx addr sg
  where
    infoMessage =
        L.logDebug $
        sformat ("Sending tx, signature to Notary: " % shown) (tx, sg)
    successMessage res =
        L.logDebug $ sformat ("Received signatures from Notary: " % shown) res

-- | Read-only method of Notary. Returns current state of signatures
-- for the given address (that implicitly defines addrids ~
-- transaction inputs) and transaction itself.
getTxSignatures :: WorkMode m => Transaction -> Address -> m [(Address, Signature)]
getTxSignatures tx addr =
    withResult infoMessage successMessage $
    callNotary $ P.call (P.RSCNotary P.GetSignatures) tx addr
  where
    infoMessage =
        L.logDebug $
        sformat ("Getting signatures for tx " % shown % ", addr " % shown) tx addr
    successMessage res =
        L.logDebug $ sformat ("Received signatures from Notary: " % shown) res

-- | This method is somewhat mystic because it's not used anywhere and
-- it won't be until we have perfectly working UI. It's supposed to be
-- used over time to detect transactions that you `may` want to
-- sign. And then dialog pops up.
pollTxsFromNotary
    :: WorkMode m
    => [Address] -> m [(Address, [(Transaction, [(Address, Signature)])])]
pollTxsFromNotary addrs =
    withResult infoMessage successMessage $
    callNotary $ P.call (P.RSCNotary P.PollTransactions) addrs
  where
    infoMessage =
        L.logDebug $
        sformat
            ("Polling transactions to sign for addresses: " % shown)
            addrs
    successMessage res =
        L.logDebug $ sformat ("Received transactions to sign: " % shown) res
