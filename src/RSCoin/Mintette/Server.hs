{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve

       , handlePeriodFinished
       , handleNewPeriod
       , handleCheckTx
       , handleCheckTxBatch
       , handleCommitTx
       , handleGetMintettePeriod
       , handleGetUtxo
       , handleGetLogs
       ) where

import           Control.Lens              (view)
import           Control.Monad             (unless)
import           Control.Monad.Catch       (catch, throwM, try)
import           Control.Monad.Extra       (unlessM)
import           Control.Monad.Trans       (lift)
import           Data.Bifunctor            (first)
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import           Formatting                (build, int, sformat, (%))

import           Serokell.Util.Text        (listBuilderJSON,
                                            listBuilderJSONIndent, pairBuilder,
                                            show')

import           Control.TimeWarp.Rpc      (ServerT, serverTypeRestriction0,
                                            serverTypeRestriction1,
                                            serverTypeRestriction2,
                                            serverTypeRestriction3)
import qualified RSCoin.Core               as C

import           RSCoin.Mintette.Acidic    (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            GetLogs (..), GetPeriodId (..),
                                            GetPreviousMintetteId (..),
                                            GetUtxoPset (..), StartPeriod (..),
                                            tidyState)
import           RSCoin.Mintette.AcidState (State, query, update)
import           RSCoin.Mintette.Env       (RuntimeEnv)
import           RSCoin.Mintette.Error     (MintetteError (..),
                                            logMintetteError)

serve :: C.WorkMode m => Int -> State -> RuntimeEnv -> m ()
serve port st env = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction3
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction2
    idr6 <- serverTypeRestriction0
    idr7 <- serverTypeRestriction0
    idr8 <- serverTypeRestriction1
    C.serve port
        [ C.method (C.RSCMintette C.PeriodFinished) $
            idr1 $ handlePeriodFinished env st
        , C.method (C.RSCMintette C.AnnounceNewPeriod) $
            idr2 $ handleNewPeriod env st
        , C.method (C.RSCMintette C.CheckTx) $
            idr3 $ handleCheckTx env st
        , C.method (C.RSCMintette C.CheckTxBatch) $
            idr4 $ handleCheckTxBatch env st
        , C.method (C.RSCMintette C.CommitTx) $
            idr5 $ handleCommitTx env st
        , C.method (C.RSCMintette C.GetMintettePeriod) $
            idr6 $ handleGetMintettePeriod st
        , C.method (C.RSCDump C.GetMintetteUtxo) $
            idr7 $ handleGetUtxo st
        , C.method (C.RSCDump C.GetMintetteLogs) $
            idr8 $ handleGetLogs st
        ]

type ServerTE m a = ServerT m (Either T.Text a)

toServer :: C.WorkMode m => m a -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: MintetteError) = do
        C.logError $ show' e
        return $ Left $ show' e

handlePeriodFinished
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> C.WithSignature C.PeriodId
    -> ServerTE m C.PeriodResult
handlePeriodFinished env st signed =
    toServer $
    do bankPublicKey <- view C.bankPublicKey <$> C.getNodeContext
       unless (C.verifyWithSignature bankPublicKey signed) $
           throwM MEInvalidBankSignature
       let pId = C.wsValue signed
       (curUtxo, curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("Before period end utxo is: " % build % "\nCurrent pset is: " %
                build)
               curUtxo
               curPset
       C.logInfo $ sformat ("Period " % int % " has just finished!") pId
       res@(_, blks, lgs) <- update st $ FinishPeriod pId env
       C.logInfo $
           sformat
               ("Here is PeriodResult:\n Blocks: " % build % "\n Logs: " % build %
                "\n")
               (listBuilderJSONIndent 2 blks)
               lgs
       (curUtxo', curPset') <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("After period end utxo is: " % build % "\nCurrent pset is: " %
                build)
               curUtxo'
               curPset'
       tidyState st
       return res

handleNewPeriod
    :: C.WorkMode m
    => RuntimeEnv -> State -> C.WithSignature C.NewPeriodData -> ServerTE m ()
handleNewPeriod env st signed =
    toServer $
    do bankPublicKey <- view C.bankPublicKey <$> C.getNodeContext
       unless (C.verifyWithSignature bankPublicKey signed) $
           throwM MEInvalidBankSignature
       let npd = C.wsValue signed
       prevMid <- query st GetPreviousMintetteId
       C.logInfo $
           sformat
               ("New period has just started, I am mintette #" % build %
                " (prevId).\nHere is new period data:\n " % build)
               prevMid npd
       update st $ StartPeriod npd env
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("After start of new period, my utxo: " % build %
               "\nCurrent pset is: " % build)
               curUtxo curPset

handleCheckTx
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> C.Transaction
    -> C.AddrId
    -> [(C.Address, C.Signature C.Transaction)]
    -> ServerTE m C.CheckConfirmation
handleCheckTx env st tx addrId sg =
    toServer $
    do C.logDebug $
           sformat ("Checking addrid (" % build % ") from transaction: " % build)
               addrId tx
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("My current utxo is: " % build % "\nCurrent pset is: " % build)
               curUtxo curPset
       res <- update st $ CheckNotDoubleSpent tx addrId sg env
       C.logInfo $
            sformat ("Confirmed addrid (" % build % ") from transaction: " % build)
                addrId tx
       C.logInfo $ sformat ("Confirmation: " % build) res
       return res

handleCheckTxBatch
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> C.Transaction
    -> M.Map C.AddrId [(C.Address, C.Signature C.Transaction)]
    -> ServerTE m (M.Map C.AddrId (Either T.Text C.CheckConfirmation))
handleCheckTxBatch env st tx sigs =
    toServer $
    do C.logDebug $ sformat
           ("Checking addrids " % build % "of transaction: " % build)
           (listBuilderJSON $ M.keys sigs) tx
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("My current utxo is: " % build % "\nCurrent pset is: " % build)
               curUtxo curPset
       res <- M.fromList <$>
           mapM (\(addrId, sig) ->
                  (addrId,) <$>
                  try' (update st $ CheckNotDoubleSpent tx addrId sig env))
           (M.assocs sigs)
       C.logInfo "Returning confirmations"-- TODO add logging
       return res
  where
    try' :: (C.WorkMode m) => m a -> m (Either T.Text a)
    try' action = do
        (res :: Either MintetteError a) <- try action
        return $ first show' res

handleCommitTx
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> C.Transaction
    -> C.CheckConfirmations
    -> ServerTE m C.CommitAcknowledgment
handleCommitTx env st tx cc =
    toServer $
    do C.logDebug $
           sformat ("There is an attempt to commit transaction (" % build % ").") tx
       C.logDebug $ sformat ("Here are confirmations: " % build) cc
       res <- update st $ CommitTx tx cc env
       C.logInfo $ sformat ("Successfully committed transaction " % build) tx
       return res

handleGetMintettePeriod
    :: C.WorkMode m
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

handleGetUtxo :: C.WorkMode m => State -> ServerTE m C.Utxo
handleGetUtxo st =
    toServer $
    do unlessM C.isTestRun $
           throwM $ C.BadRequest "getMintetteUtxo is only available in test run"
       C.logDebug "Getting utxo"
       (curUtxo, _) <- query st GetUtxoPset
       C.logDebug $ sformat ("Corrent utxo is: " % build) curUtxo
       return curUtxo

handleGetLogs
    :: C.WorkMode m
    => State -> C.PeriodId -> ServerTE m (Maybe C.ActionLog)
handleGetLogs st pId =
    toServer $
    do res <- query st $ GetLogs pId
       C.logDebug $
            sformat ("Getting logs for periodId " % int % ": " % build)
                pId (listBuilderJSONIndent 2 . map pairBuilder <$> res)
       return res
