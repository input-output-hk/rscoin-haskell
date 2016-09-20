{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for mintette's data (configurable).
-- WARNING: this module is outdated! Rewrite it when needed.

module Test.RSCoin.Full.Mintette.Storage
       ( checkNotDoubleSpent
       , commitTx
       ) where

import           Control.Lens                     (at, use, uses, view, (%=), (.=), (<>=),
                                                   _1)
import           Control.Monad                    (unless, when)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.Reader             (ReaderT)
import           Control.Monad.State.Class        (MonadState)
import qualified Data.HashMap.Strict              as HM
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe, isJust)
import qualified Data.Set                         as S
import           Safe                             (atMay)

import           RSCoin.Core                      (AddrId, SecretKey, Transaction (..),
                                                   TxStrategy (..), computeOutputAddrids,
                                                   derivePublicKey, isStrategyCompleted,
                                                   mkCheckConfirmation, owners, sign,
                                                   verifyCheckConfirmation)
import qualified RSCoin.Core                      as C
import           RSCoin.Mintette.Env              (RuntimeEnv, reSecretKey)
import           RSCoin.Mintette.Error            (MintetteError (..))
import           RSCoin.Mintette.Storage          (Storage, addresses, checkIsActive,
                                                   checkTxSum, curMintetteId, dpk,
                                                   getLogHead, logHeads, logSize,
                                                   mintettes, periodId, pset,
                                                   pushLogEntry, readerToState, txset,
                                                   utxo, utxoAdded, utxoDeleted)

import           Test.RSCoin.Full.Mintette.Config (MintetteConfig (..))

type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a
type ExceptUpdateInEnv a = forall m . (MonadThrow m, MonadState Storage m) => ReaderT RuntimeEnv m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: MintetteConfig
                    -> Transaction
                    -> AddrId
                    -> [(C.Address, C.Signature C.Transaction)]
                    -> ExceptUpdateInEnv C.CheckConfirmation
checkNotDoubleSpent conf tx addrId sg = do
    unless (checkActive conf) checkIsActive
    if ignoreCheckTx conf then finalize else verify
  where
    verify = do
        unless (ignoreSumCheckTx conf) $ checkTxSum tx
        unless (addrId `elem` txInputs tx || ignoreAddridInTxCheckTx conf) $
            throwM $ MEInconsistentRequest "AddrId is not part of inputs"
        inPset <- HM.lookup addrId <$> use pset
        maybe notInPsetCase inPsetCase inPset
    inPsetCase storedTx
      | storedTx == tx = finishCheck
      | otherwise = throwM $ MENotUnspent addrId
    notInPsetCase = do
          addr <- HM.lookup addrId <$> use utxo
          maybe (throwM $ MENotUnspent addrId) checkSignaturesAndFinish addr
    checkSignaturesAndFinish addr = do
           strategy <- uses addresses $ fromMaybe DefaultStrategy . M.lookup addr
           if isStrategyCompleted strategy addr sg tx
                            then finishCheck
                            else throwM MEInvalidSignature
    finishCheck
      | updateUtxoCheckTx conf = do
        pushLogEntry $ C.QueryEntry tx
        utxoEntry <- uses utxo (HM.lookup addrId)
        utxo %= HM.delete addrId
        when (isJust utxoEntry)
             (utxoDeleted %= HM.insert addrId (fromJust utxoEntry))
        pset %= HM.insert addrId tx
        finalize
      | otherwise = finalize
    finalize = do
        pId <- use periodId
        hsh <- snd . fromJust <$> readerToState getLogHead
        logSz <- use logSize
        sk <- view reSecretKey
        if inverseCheckTx conf
        then throwM $ MENotUnspent addrId
        else return $ mkCheckConfirmation sk tx addrId (hsh, logSz - 1) pId

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.  If it's true, add transaction to storage and
-- return signed confirmation.
commitTx :: MintetteConfig
         -> Transaction
         -> C.CheckConfirmations
         -> ExceptUpdateInEnv C.CommitAcknowledgment
commitTx conf tx@Transaction{..} bundle = do
    unless (checkActive conf) checkIsActive
    checkTxSum tx
    mts <- use mintettes
    mId <- fromJust <$> use curMintetteId
    unless (C.isOwner mts (C.hash tx) mId) $ throwM $
        MEInconsistentRequest "I'm not an owner!"
    curDpk <- use dpk
    isConfirmed <- and <$> mapM (checkInputConfirmed mts curDpk) txInputs
    sk <- view reSecretKey
    res <- commitTxChecked conf isConfirmed sk tx bundle
    mapM_ (updateLogHeads curDpk) $ M.assocs bundle
    return res
  where
    checkInputConfirmed _ _ _
      | skipChecksCommitTx conf = return True
    checkInputConfirmed mts curDpk addrid = do
        pId <- use periodId
        let addridOwners = owners mts (view _1 addrid)
            ownerConfirmed owner =
                maybe
                    False
                    (\proof ->
                          verifyCheckConfirmation proof tx addrid pId &&
                          verifyDpk curDpk owner proof) $
                M.lookup (owner, addrid) bundle
            filtered = filter ownerConfirmed addridOwners
        return (length filtered > length addridOwners `div` 2)
    verifyDpk _ _ _ | skipDpkVerificationCommitTx conf = True
    verifyDpk curDpk ownerId C.CheckConfirmation{..} =
        maybe
            False
            (\(k,_) ->
                  ccMintetteKey == k) $
        curDpk `atMay`
        ownerId
    updateLogHeads curDpk ((mId,_),C.CheckConfirmation{..}) =
        maybe (return ()) (updateLogHeadsDo mId ccHead . fst) $ curDpk `atMay`
        mId
    updateLogHeadsDo mId lh pk = do
        myId <- fromJust <$> use curMintetteId
        unless (mId == myId) $ logHeads . at pk .= Just lh

commitTxChecked
    :: MintetteConfig
    -> Bool
    -> SecretKey
    -> Transaction
    -> C.CheckConfirmations
    -> ExceptUpdate C.CommitAcknowledgment
commitTxChecked _ False _ _ _ = throwM MENotConfirmed
commitTxChecked conf True sk tx bundle = do
    pushLogEntry $ C.CommitEntry tx bundle
    when (updateUtxoCommitTx conf) $ do
        utxo <>= HM.fromList (computeOutputAddrids tx)
        utxoAdded <>= HM.fromList (computeOutputAddrids tx)
        txset %= S.insert tx
    let pk = derivePublicKey sk
    hsh <- snd . fromJust <$> readerToState getLogHead
    logSz <- use logSize
    let logChainHead = (hsh, logSz)
    return $ C.CommitAcknowledgment pk (sign sk (tx, logChainHead)) logChainHead
