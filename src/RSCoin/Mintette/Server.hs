{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       , handlePeriodFinished
       , handleNewPeriod
       , handleCheckTx
       , handleCommitTx
       , handleGetUtxo
       , handleGetBlocks
       , handleGetLogs
       ) where

import           Control.Exception         (throwIO, try)
import           Control.Monad.Catch       (catch, throwM)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Acid.Advanced        (query', update')
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)

import           Serokell.Util.Text        (format', formatSingle',
                                            listBuilderJSONIndent, pairBuilder,
                                            show')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.Acidic    (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            GetBlocks (..), GetLogs (..),
                                            GetUtxoPset (..),
                                            PreviousMintetteId (..),
                                            StartPeriod (..))
import           RSCoin.Mintette.AcidState (State)
import           RSCoin.Mintette.Error     (MintetteError (..))
import           RSCoin.Timed              (ServerT, WorkMode,
                                            serverTypeRestriction0,
                                            serverTypeRestriction1,
                                            serverTypeRestriction3)

logError, logWarning, logInfo, logDebug :: MonadIO m => Text -> m ()
logError = C.logError C.mintetteLoggerName
logWarning = C.logWarning C.mintetteLoggerName
logInfo = C.logInfo C.mintetteLoggerName
logDebug = C.logDebug C.mintetteLoggerName

serve :: WorkMode m => Int -> State -> C.SecretKey -> m ()
serve port st sk = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction3
    idr4 <- serverTypeRestriction3
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction1
    idr7 <- serverTypeRestriction1
    C.serve port
        [ C.method (C.RSCMintette C.PeriodFinished) $
            idr1 $ handlePeriodFinished sk st
        , C.method (C.RSCMintette C.AnnounceNewPeriod) $
            idr2 $ handleNewPeriod st
        , C.method (C.RSCMintette C.CheckTx) $
            idr3 $ handleCheckTx sk st
        , C.method (C.RSCMintette C.CommitTx) $
            idr4 $ handleCommitTx sk st
        , C.method (C.RSCDump C.GetMintetteUtxo) $
            idr5 $ handleGetUtxo st
        , C.method (C.RSCDump C.GetMintetteBlocks) $
            idr6 $ handleGetBlocks st
        , C.method (C.RSCDump C.GetMintetteLogs) $
            idr7 $ handleGetLogs st
        ]

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: MintetteError) = do
        logError $ show' e
        throwIO e

handlePeriodFinished
    :: WorkMode m
    => C.SecretKey -> State -> C.PeriodId -> ServerT m C.PeriodResult
handlePeriodFinished sk st pId =
    toServer $
    do (curUtxo,curPset) <- query' st GetUtxoPset
       logDebug $
           format'
               "Before period end utxo is: {}\nCurrent pset is: {}"
               (curUtxo, curPset)
       logInfo $ formatSingle' "Period {} has just finished!" pId
       res@(_,blks,lgs) <- update' st $ FinishPeriod sk pId
       logInfo $
           format'
               "Here is PeriodResult:\n Blocks: {}\n Logs: {}\n"
               (listBuilderJSONIndent 2 blks, lgs)
       (curUtxo', curPset') <- query' st GetUtxoPset
       logDebug $
           format'
               "After period end utxo is: {}\nCurrent pset is: {}"
               (curUtxo', curPset')
       return res

handleNewPeriod :: WorkMode m
                => State
                -> C.NewPeriodData
                -> ServerT m ()
handleNewPeriod st npd =
    toServer $
    do prevMid <- query' st PreviousMintetteId
       logInfo $
           format'
               ("New period has just started, I am mintette #{} (prevId).\n" <>
                "Here is new period data:\n {}")
               (prevMid, npd)
       update' st $ StartPeriod npd
       (curUtxo,curPset) <- query' st GetUtxoPset
       logDebug $
           format'
               "After start of new period, my utxo: {}\nCurrent pset is: {}"
               (curUtxo, curPset)

handleCheckTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.AddrId
    -> C.Signature
    -> ServerT m (Either MintetteError C.CheckConfirmation)
handleCheckTx sk st tx addrId sg =
    toServer $
    do logDebug $
           format' "Checking addrid ({}) from transaction: {}" (addrId, tx)
       (curUtxo,curPset) <- query' st GetUtxoPset
       logDebug $
           format'
               "My current utxo is: {}\nCurrent pset is: {}"
               (curUtxo, curPset)
       res <- try $ update' st $ CheckNotDoubleSpent sk tx addrId sg
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        logWarning $ formatSingle' "CheckTx failed: {}" e
        return $ Left e
    onSuccess res = do
        logInfo $
            format' "Confirmed addrid ({}) from transaction: {}" (addrId, tx)
        logInfo $ formatSingle' "Confirmation: {}" res
        return $ Right res

handleCommitTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> ServerT m (Either MintetteError C.CommitConfirmation)
handleCommitTx sk st tx pId cc =
    toServer $
    do logDebug $
           format'
               "There is an attempt to commit transaction ({}), provided periodId is {}."
               (tx, pId)
       logDebug $ formatSingle' "Here are confirmations: {}" cc
       res <- try $ update' st $ CommitTx sk tx pId cc
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        logWarning $ formatSingle' "CommitTx failed: {}" e
        return $ Left e
    onSuccess res = do
        logInfo $ formatSingle' "Successfully committed transaction {}" tx
        return $ Right res

-- Dumping Mintette state

handleGetUtxo :: WorkMode m => State -> ServerT m C.Utxo
handleGetUtxo st =
    toServer $
    do logInfo "Getting utxo"
       (curUtxo, _) <- query' st GetUtxoPset
       logDebug $ formatSingle' "Corrent utxo is: {}" curUtxo
       return curUtxo

handleGetBlocks :: WorkMode m
                => State -> C.PeriodId -> ServerT m (Maybe [C.LBlock])
handleGetBlocks st pId =
    toServer $
    do res <- query' st $ GetBlocks pId
       logInfo $
            format' "Getting blocks for periodId {}: {}" (pId, listBuilderJSONIndent 2 <$> res)
       return res

-- TODO: code duplication, simiar as handleGetBlocks => refactor!
handleGetLogs :: WorkMode m
              => State -> C.PeriodId -> ServerT m (Maybe C.ActionLog)
handleGetLogs st pId =
    toServer $
    do res <- query' st $ GetLogs pId
       logInfo $
            format' "Getting logs for periodId {}: {}" (pId, listBuilderJSONIndent 2 . map pairBuilder <$> res)
       return res
