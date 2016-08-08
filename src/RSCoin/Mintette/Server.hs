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
import           Control.Monad.Catch       (catch)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Acid.Advanced        (query', update')
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Formatting                (int, sformat, (%))

import           Serokell.Util.Text        (format', formatSingle',
                                            listBuilderJSONIndent, pairBuilder,
                                            show')


import qualified RSCoin.Core               as C
import           RSCoin.Mintette.Acidic    (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            GetBlocks (..), GetLogs (..),
                                            GetPeriodId (..), GetUtxoPset (..),
                                            PreviousMintetteId (..),
                                            StartPeriod (..))
import           RSCoin.Mintette.AcidState (State)
import           RSCoin.Mintette.Error     (MintetteError (..))
import           RSCoin.Timed              (ServerT, WorkMode,
                                            serverTypeRestriction0,
                                            serverTypeRestriction1,
                                            serverTypeRestriction2,
                                            serverTypeRestriction3)

serve :: WorkMode m => Int -> State -> C.SecretKey -> m ()
serve port st sk = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction3
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction0
    idr7 <- serverTypeRestriction1
    idr8 <- serverTypeRestriction1
    C.serve port
        [ C.method (C.RSCMintette C.PeriodFinished) $
            idr1 $ handlePeriodFinished sk st
        , C.method (C.RSCMintette C.AnnounceNewPeriod) $
            idr2 $ handleNewPeriod st
        , C.method (C.RSCMintette C.CheckTx) $
            idr3 $ handleCheckTx sk st
        , C.method (C.RSCMintette C.CommitTx) $
            idr4 $ handleCommitTx sk st
        , C.method (C.RSCMintette C.GetMintettePeriod) $
            idr5 $ handleGetMintettePeriod st
        , C.method (C.RSCDump C.GetMintetteUtxo) $
            idr6 $ handleGetUtxo st
        , C.method (C.RSCDump C.GetMintetteBlocks) $
            idr7 $ handleGetBlocks st
        , C.method (C.RSCDump C.GetMintetteLogs) $
            idr8 $ handleGetLogs st
        ]

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: MintetteError) = do
        C.logError $ show' e
        throwIO e

handlePeriodFinished
    :: WorkMode m
    => C.SecretKey -> State -> C.PeriodId -> ServerT m C.PeriodResult
handlePeriodFinished sk st pId =
    toServer $
    do (curUtxo,curPset) <- query' st GetUtxoPset
       C.logDebug $
           format'
               "Before period end utxo is: {}\nCurrent pset is: {}"
               (curUtxo, curPset)
       C.logInfo $ formatSingle' "Period {} has just finished!" pId
       res@(_,blks,lgs) <- update' st $ FinishPeriod sk pId
       C.logInfo $
           format'
               "Here is PeriodResult:\n Blocks: {}\n Logs: {}\n"
               (listBuilderJSONIndent 2 blks, lgs)
       (curUtxo', curPset') <- query' st GetUtxoPset
       C.logDebug $
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
       C.logInfo $
           format'
               ("New period has just started, I am mintette #{} (prevId).\n" <>
                "Here is new period data:\n {}")
               (prevMid, npd)
       update' st $ StartPeriod npd
       (curUtxo,curPset) <- query' st GetUtxoPset
       C.logDebug $
           format'
               "After start of new period, my utxo: {}\nCurrent pset is: {}"
               (curUtxo, curPset)

logFunction :: (MonadIO m, C.WithNamedLogger m) => MintetteError -> Text -> m ()
logFunction MEInactive = C.logInfo
logFunction _ = C.logWarning

handleCheckTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.AddrId
    -> [(C.Address, C.Signature)]
    -> ServerT m (Either MintetteError C.CheckConfirmation)
handleCheckTx sk st tx addrId sg =
    toServer $
    do C.logDebug $
           format' "Checking addrid ({}) from transaction: {}" (addrId, tx)
       (curUtxo,curPset) <- query' st GetUtxoPset
       C.logDebug $
           format'
               "My current utxo is: {}\nCurrent pset is: {}"
               (curUtxo, curPset)
       res <- try $ update' st $ CheckNotDoubleSpent sk tx addrId sg
       either onError onSuccess res
  where
    onError e =
        Left e <$ (logFunction e $ formatSingle' "CheckTx failed: {}" e)
    onSuccess res = do
        C.logInfo $
            format' "Confirmed addrid ({}) from transaction: {}" (addrId, tx)
        C.logInfo $ formatSingle' "Confirmation: {}" res
        return $ Right res

handleCommitTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.CheckConfirmations
    -> ServerT m (Either MintetteError C.CommitAcknowledgment)
handleCommitTx sk st tx cc =
    toServer $
    do C.logDebug $
           formatSingle' "There is an attempt to commit transaction ({})." tx
       C.logDebug $ formatSingle' "Here are confirmations: {}" cc
       res <- try $ update' st $ CommitTx sk tx cc
       either onError onSuccess res
  where
    onError e =
        Left e <$ (logFunction e $ formatSingle' "CommitTx failed: {}" e)
    onSuccess res = do
        C.logInfo $ formatSingle' "Successfully committed transaction {}" tx
        return $ Right res

handleGetMintettePeriod :: WorkMode m => State -> ServerT m (Maybe C.PeriodId)
handleGetMintettePeriod st =
    toServer $
    do C.logDebug "Querying periodId"
       res <- try $ query' st GetPeriodId
       either onError onSuccess res
  where
    onError e = do
        logFunction e "Failed to query periodId"
        return Nothing
    onSuccess pid = do
        C.logInfo $ sformat ("Successfully returning periodId " % int) pid
        return $ Just pid


-- Dumping Mintette state

handleGetUtxo :: WorkMode m => State -> ServerT m C.Utxo
handleGetUtxo st =
    toServer $
    do C.logDebug "Getting utxo"
       (curUtxo, _) <- query' st GetUtxoPset
       C.logDebug $ formatSingle' "Corrent utxo is: {}" curUtxo
       return curUtxo

handleGetBlocks :: WorkMode m
                => State -> C.PeriodId -> ServerT m (Maybe [C.LBlock])
handleGetBlocks st pId =
    toServer $
    do res <- query' st $ GetBlocks pId
       C.logDebug $
            format' "Getting blocks for periodId {}: {}" (pId, listBuilderJSONIndent 2 <$> res)
       return res

-- TODO: code duplication, simiar as handleGetBlocks => refactor!
handleGetLogs :: WorkMode m
              => State -> C.PeriodId -> ServerT m (Maybe C.ActionLog)
handleGetLogs st pId =
    toServer $
    do res <- query' st $ GetLogs pId
       C.logDebug $
            format' "Getting logs for periodId {}: {}" (pId, listBuilderJSONIndent 2 . map pairBuilder <$> res)
       return res
