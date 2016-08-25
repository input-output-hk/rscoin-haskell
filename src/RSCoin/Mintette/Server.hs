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

import           Control.Monad.Catch       (catch, try)
import           Control.Monad.Trans       (lift, liftIO)
import qualified Data.Text                 as T
import           Formatting                (build, int, sformat, (%))

import           Serokell.Util.Text        (listBuilderJSONIndent, pairBuilder,
                                            show')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.Acidic    (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            GetBlocks (..), GetLogs (..),
                                            GetPeriodId (..), GetUtxoPset (..),
                                            PreviousMintetteId (..),
                                            StartPeriod (..), tidyState)
import           RSCoin.Mintette.AcidState (State, query, update)
import           RSCoin.Mintette.Error     (MintetteError (..),
                                            logMintetteError)
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

type ServerTE m a = ServerT m (Either T.Text a)

toServer :: WorkMode m => m a -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: MintetteError) = do
        C.logError $ show' e
        return $ Left $ show' e

handlePeriodFinished
    :: WorkMode m
    => C.SecretKey -> State -> C.PeriodId -> ServerTE m C.PeriodResult
handlePeriodFinished sk st pId =
    toServer $
    do (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("Before period end utxo is: " % build %
               "\nCurrent pset is: " % build)
               curUtxo curPset
       C.logInfo $ sformat ("Period " % int % " has just finished!") pId
       res@(_,blks,lgs) <- update st $ FinishPeriod sk pId
       C.logInfo $
           sformat
               ("Here is PeriodResult:\n Blocks: " % build %
                "\n Logs: " % build % "\n")
               (listBuilderJSONIndent 2 blks) lgs
       (curUtxo', curPset') <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("After period end utxo is: " % build %
                "\nCurrent pset is: " % build)
               curUtxo' curPset'
       tidyState st
       return res

handleNewPeriod
    :: WorkMode m
    => State -> C.NewPeriodData -> ServerTE m ()
handleNewPeriod st npd =
    toServer $
    do prevMid <- query st PreviousMintetteId
       C.logInfo $
           sformat
               ("New period has just started, I am mintette #" % build %
                " (prevId).\nHere is new period data:\n " % build)
               prevMid npd
       update st $ StartPeriod npd
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("After start of new period, my utxo: " % build %
               "\nCurrent pset is: " % build)
               curUtxo curPset

handleCheckTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.AddrId
    -> [(C.Address, C.Signature C.Transaction)]
    -> ServerTE m C.CheckConfirmation
handleCheckTx sk st tx addrId sg =
    toServer $
    do C.logDebug $
           sformat ("Checking addrid (" % build % ") from transaction: " % build)
               addrId tx
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("My current utxo is: " % build % "\nCurrent pset is: " % build)
               curUtxo curPset
       res <- update st $ CheckNotDoubleSpent sk tx addrId sg
       C.logInfo $
            sformat ("Confirmed addrid (" % build % ") from transaction: " % build)
                addrId tx
       C.logInfo $ sformat ("Confirmation: " % build) res
       return res

handleCommitTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> C.Transaction
    -> C.CheckConfirmations
    -> ServerTE m C.CommitAcknowledgment
handleCommitTx sk st tx cc =
    toServer $
    do C.logDebug $
           sformat ("There is an attempt to commit transaction (" % build % ").") tx
       C.logDebug $ sformat ("Here are confirmations: " % build) cc
       res <- update st $ CommitTx sk tx cc
       C.logInfo $ sformat ("Successfully committed transaction " % build) tx
       return res

handleGetMintettePeriod
    :: WorkMode m
    => State -> ServerTE m (Maybe C.PeriodId)
handleGetMintettePeriod st =
    toServer $
    do C.logDebug "Querying periodId"
       res <- try $ query st GetPeriodId
       either onError onSuccess res
  where
    onError e = do
        logMintetteError e "Failed to query periodId"
        return Nothing
    onSuccess pid = do
        C.logInfo $ sformat ("Successfully returning periodId " % int) pid
        return $ Just pid


-- Dumping Mintette state

handleGetUtxo :: WorkMode m => State -> ServerTE m C.Utxo
handleGetUtxo st =
    toServer $
    do C.logDebug "Getting utxo"
       (curUtxo,_) <- query st GetUtxoPset
       C.logDebug $ sformat ("Corrent utxo is: " % build) curUtxo
       return curUtxo

handleGetBlocks
    :: WorkMode m
    => State -> C.PeriodId -> ServerTE m (Maybe [C.LBlock])
handleGetBlocks st pId =
    toServer $
    do res <- query st $ GetBlocks pId
       C.logDebug $
            sformat ("Getting blocks for periodId " % int % ": " % build)
                pId (listBuilderJSONIndent 2 <$> res)
       return res

handleGetLogs
    :: WorkMode m
    => State -> C.PeriodId -> ServerTE m (Maybe C.ActionLog)
handleGetLogs st pId =
    toServer $
    do res <- query st $ GetLogs pId
       C.logDebug $
            sformat ("Getting logs for periodId " % int % ": " % build)
                pId (listBuilderJSONIndent 2 . map pairBuilder <$> res)
       return res
