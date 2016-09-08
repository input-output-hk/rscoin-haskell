{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for mintette

module Test.RSCoin.Full.Mintette.Server
       ( serve
       ) where

import           Control.Exception                (throwIO, try)
import           Control.Monad.Catch              (catch)
import           Control.Monad.IO.Class           (liftIO)
import           Formatting                       (build, sformat, (%))

import           Serokell.Util.Text               (show')

import           Control.TimeWarp.Rpc             (ServerT,
                                                   serverTypeRestriction0,
                                                   serverTypeRestriction1,
                                                   serverTypeRestriction2,
                                                   serverTypeRestriction3)
import qualified RSCoin.Core                      as C
import           RSCoin.Mintette.Acidic           (GetUtxoPset (..))
import           RSCoin.Mintette.AcidState        (State, query, update)
import           RSCoin.Mintette.Env              (RuntimeEnv)
import           RSCoin.Mintette.Error            (MintetteError)
import qualified RSCoin.Mintette.Server           as OMS

import qualified Test.RSCoin.Full.Mintette.Acidic as MA
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig)

-- | Serve as mintette according to mintette config provided
serve
    :: C.WorkMode m
    => MintetteConfig -> Int -> State -> RuntimeEnv -> m ()
serve conf port st env = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction3
    idr4 <- serverTypeRestriction2
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction1
    C.serve port
        [ C.method (C.RSCMintette C.PeriodFinished) $
            idr1 $ OMS.handlePeriodFinished env st
        , C.method (C.RSCMintette C.AnnounceNewPeriod) $
            idr2 $ OMS.handleNewPeriod env st
        , C.method (C.RSCMintette C.CheckTx) $
            idr3 $ handleCheckTx env st conf
        , C.method (C.RSCMintette C.CommitTx) $
            idr4 $ handleCommitTx env st conf
        , C.method (C.RSCDump C.GetMintetteUtxo) $
            idr5 $ OMS.handleGetUtxo st
        , C.method (C.RSCDump C.GetMintetteLogs) $
            idr6 $ OMS.handleGetLogs st
        ]

toServer :: C.WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: MintetteError) = do
        C.logError $ show' e
        throwIO e

handleCheckTx
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> MintetteConfig
    -> C.Transaction
    -> C.AddrId
    -> [(C.Address, C.Signature C.Transaction)]
    -> ServerT m (Maybe C.CheckConfirmation)
handleCheckTx env st conf tx addrId sg =
    toServer $
    do C.logDebug $
           sformat ("Checking addrid (" % build % ") from transaction: " % build) addrId tx
       (curUtxo,curPset) <- query st GetUtxoPset
       C.logDebug $
           sformat
               ("My current utxo is: " % build % "\nCurrent pset is: " % build)
               curUtxo curPset
       res <- try $ update st $ MA.CheckNotDoubleSpent conf tx addrId sg env
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        C.logWarning $ sformat ("CheckTx failed: " % build) e
        return Nothing
    onSuccess res = do
        C.logInfo $
            sformat ("Confirmed addrid (" % build %
                     ") from transaction: " % build) addrId tx
        C.logInfo $ sformat ("Confirmation: " % build) res
        return $ Just res

handleCommitTx
    :: C.WorkMode m
    => RuntimeEnv
    -> State
    -> MintetteConfig
    -> C.Transaction
    -> C.CheckConfirmations
    -> ServerT m (Maybe C.CommitAcknowledgment)
handleCommitTx env st conf tx cc =
    toServer $
    do C.logDebug $
           sformat ("There is an attempt to commit transaction (" % build % ")") tx
       C.logDebug $ sformat ("Here are confirmations: " % build) cc
       res <- try $ update st $ MA.CommitTx conf tx cc env
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        C.logWarning $ sformat ("CommitTx failed: " % build) e
        return Nothing
    onSuccess res = do
        C.logInfo $ sformat ("Successfully committed transaction " % build) tx
        return $ Just res
