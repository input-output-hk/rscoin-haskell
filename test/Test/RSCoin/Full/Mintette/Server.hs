{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation for mintette

module Test.RSCoin.Full.Mintette.Server
       ( serve
       ) where

import           Control.Exception                (throwIO, try)
import           Control.Monad.Catch              (catch)
import           Control.Monad.IO.Class           (liftIO)
import           Data.Acid.Advanced               (query', update')

import           Serokell.Util.Text               (format', formatSingle',
                                                   show')

import qualified RSCoin.Core                      as C
import           RSCoin.Mintette.Acidic           (GetUtxoPset (..))
import           RSCoin.Mintette.AcidState        (State)
import           RSCoin.Mintette.Error            (MintetteError)
import qualified RSCoin.Mintette.Server           as OMS
import           RSCoin.Timed                     (ServerT, WorkMode,
                                                   serverTypeRestriction0,
                                                   serverTypeRestriction1,
                                                   serverTypeRestriction3)

import qualified Test.RSCoin.Full.Mintette.Acidic as MA
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig)

-- | Serve as mintette according to mintette config provided
serve
    :: WorkMode m
    => MintetteConfig -> Int -> State -> C.SecretKey -> m ()
serve conf port st sk = do
    idr1 <- serverTypeRestriction1
    idr2 <- serverTypeRestriction1
    idr3 <- serverTypeRestriction3
    idr4 <- serverTypeRestriction3
    idr5 <- serverTypeRestriction0
    idr6 <- serverTypeRestriction1
    idr7 <- serverTypeRestriction1
    C.serve port
        [ C.method (C.RSCMintette C.PeriodFinished) $
            idr1 $ OMS.handlePeriodFinished sk st
        , C.method (C.RSCMintette C.AnnounceNewPeriod) $
            idr2 $ OMS.handleNewPeriod st
        , C.method (C.RSCMintette C.CheckTx) $
            idr3 $ handleCheckTx sk st conf
        , C.method (C.RSCMintette C.CommitTx) $
            idr4 $ handleCommitTx sk st conf
        , C.method (C.RSCDump C.GetMintetteUtxo) $
            idr5 $ OMS.handleGetUtxo st
        , C.method (C.RSCDump C.GetMintetteBlocks) $
            idr6 $ OMS.handleGetBlocks st
        , C.method (C.RSCDump C.GetMintetteLogs) $
            idr7 $ OMS.handleGetLogs st
        ]

toServer :: WorkMode m => IO a -> ServerT m a
toServer action = liftIO $ action `catch` handler
  where
    handler (e :: MintetteError) = do
        C.logError $ show' e
        throwIO e

handleCheckTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> MintetteConfig
    -> C.Transaction
    -> C.AddrId
    -> C.Signature
    -> ServerT m (Maybe C.CheckConfirmation)
handleCheckTx
handleCheckTx sk st conf tx addrId sg =
    toServer $
    do C.logDebug $
           format' "Checking addrid ({}) from transaction: {}" (addrId, tx)
       (curUtxo,curPset) <- query' st GetUtxoPset
       C.logDebug $
           format'
               "My current utxo is: {}\nCurrent pset is: {}"
               (curUtxo, curPset)
       res <- try $ update' st $ MA.CheckNotDoubleSpent conf sk tx addrId sg
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        C.logWarning $ formatSingle' "CheckTx failed: {}" e
        return Nothing
    onSuccess res = do
        C.logInfo $
            format' "Confirmed addrid ({}) from transaction: {}" (addrId, tx)
        C.logInfo $ formatSingle' "Confirmation: {}" res
        return $ Just res

handleCommitTx
    :: WorkMode m
    => C.SecretKey
    -> State
    -> MintetteConfig
    -> C.Transaction
    -> C.PeriodId
    -> C.CheckConfirmations
    -> ServerT m (Maybe C.CommitConfirmation)
handleCommitTx sk st conf tx pId cc =
    toServer $
    do C.logDebug $
           format'
               "There is an attempt to commit transaction ({}), provided periodId is {}."
               (tx, pId)
       C.logDebug $ formatSingle' "Here are confirmations: {}" cc
       res <- try $ update' st $ MA.CommitTx conf sk tx pId cc
       either onError onSuccess res
  where
    onError (e :: MintetteError) = do
        C.logWarning $ formatSingle' "CommitTx failed: {}" e
        return Nothing
    onSuccess res = do
        C.logInfo $ formatSingle' "Successfully committed transaction {}" tx
        return $ Just res
