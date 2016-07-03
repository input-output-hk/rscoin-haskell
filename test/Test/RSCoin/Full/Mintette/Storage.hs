{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for mintette's data (configurable).

module Test.RSCoin.Full.Mintette.Storage
       ( checkNotDoubleSpent
       , commitTx
       ) where

import           Control.Lens                     (at, use, uses, (%=), (.=),
                                                   (<>=))
import           Control.Monad                    (unless, when)
import           Control.Monad.Catch              (MonadThrow (throwM))
import           Control.Monad.State.Class        (MonadState)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, isJust)
import qualified Data.Set                         as S
import           Data.Tuple.Select                (sel1)
import           Safe                             (atMay)

import           RSCoin.Core                      (AddrId, PeriodId, SecretKey,
                                                   Signature, Strategy,
                                                   Transaction (..),
                                                   computeOutputAddrids,
                                                   derivePublicKey,
                                                   ifStrategyCompleted,
                                                   mkCheckConfirmation, owners,
                                                   sign, validateSignature,
                                                   verifyCheckConfirmation)
import qualified RSCoin.Core                      as C
import           RSCoin.Mintette.Error            (MintetteError (..))
import           RSCoin.Mintette.Storage          (Storage, checkIsActive,
                                                   checkPeriodId, checkTxSum,
                                                   dpk, logHead, logHeads,
                                                   logSize, mintetteId,
                                                   mintettes, pset,
                                                   pushLogEntry, txset, utxo,
                                                   utxoAdded, utxoDeleted)

import           Test.RSCoin.Full.Mintette.Config (MintetteConfig (..))

type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: MintetteConfig
                    -> SecretKey
                    -> Transaction
                    -> AddrId
                    -> [(C.Address, C.Signature)]
                    -> ExceptUpdate C.CheckConfirmation
checkNotDoubleSpent conf sk tx addrId sg = do
    unless (checkActive conf) checkIsActive
    if ignoreCheckTx conf then finalize else verify
  where
    verify = do
        unless (ignoreSumCheckTx conf) $ checkTxSum tx
        unless (addrId `elem` txInputs tx || ignoreAddridInTxCheckTx conf) $
            throwM $ MEInconsistentRequest "AddrId is not part of inputs"
        inPset <- M.lookup addrId <$> use pset
        maybe notInPsetCase inPsetCase inPset
    inPsetCase storedTx
      | storedTx == tx = finishCheck
      | otherwise = throwM $ MENotUnspent addrId
    notInPsetCase = do
        addr <- M.lookup addrId <$> use utxo
        maybe (throwM $ MENotUnspent addrId) checkSignaturesAndFinish addr
    -- @TODO retreive strategy from storage
    checkSignaturesAndFinish a
      | ifStrategyCompleted (undefined :: Strategy) a sg tx = finishCheck
      | otherwise = throwM MEInvalidSignature
    finishCheck
      | updateUtxoCheckTx conf = do
        pushLogEntry $ C.QueryEntry tx
        utxoEntry <- uses utxo (M.lookup addrId)
        utxo %= M.delete addrId
        when (isJust utxoEntry)
             (utxoDeleted %= M.insert addrId (fromJust utxoEntry))
        pset %= M.insert addrId tx
        finalize
      | otherwise = finalize
    finalize = do
        hsh <- uses logHead (snd . fromJust)
        logSz <- use logSize
        if inverseCheckTx conf
        then throwM $ MENotUnspent addrId
        else return $ mkCheckConfirmation sk tx addrId (hsh, logSz - 1)

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.  If it's true, add transaction to storage and
-- return signed confirmation.
commitTx :: MintetteConfig
         -> SecretKey
         -> Transaction
         -> PeriodId
         -> C.CheckConfirmations
         -> ExceptUpdate C.CommitConfirmation
commitTx conf sk tx@Transaction{..} pId bundle = do
    unless (checkActive conf) checkIsActive
    checkPeriodId pId
    checkTxSum tx
    mts <- use mintettes
    mId <- fromJust <$> use mintetteId
    unless (C.isOwner mts (C.hash tx) mId) $ throwM $
        MEInconsistentRequest "I'm not an owner!"
    curDpk <- use dpk
    isConfirmed <- and <$> mapM (checkInputConfirmed mts curDpk) txInputs
    res <- commitTxChecked conf isConfirmed sk tx bundle
    mapM_ (updateLogHeads curDpk) $ M.assocs bundle
    return res
  where
    checkInputConfirmed _ _ _
      | skipChecksCommitTx conf = return True
    checkInputConfirmed mts curDpk addrid = do
        let addridOwners = owners mts (sel1 addrid)
            ownerConfirmed owner =
                maybe
                    False
                    (\proof ->
                          verifyCheckConfirmation proof tx addrid &&
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
        myId <- fromJust <$> use mintetteId
        unless (mId == myId) $ logHeads . at pk .= Just lh

commitTxChecked
    :: MintetteConfig
    -> Bool
    -> SecretKey
    -> Transaction
    -> C.CheckConfirmations
    -> ExceptUpdate C.CommitConfirmation
commitTxChecked _ False _ _ _ = throwM MENotConfirmed
commitTxChecked conf True sk tx bundle = do
    pushLogEntry $ C.CommitEntry tx bundle
    when (updateUtxoCommitTx conf) $ do
        utxo <>= M.fromList (computeOutputAddrids tx)
        utxoAdded <>= M.fromList (computeOutputAddrids tx)
        txset %= S.insert tx
    let pk = derivePublicKey sk
    hsh <- uses logHead (snd . fromJust)
    logSz <- use logSize
    let logChainHead = (hsh, logSz)
    return (pk, sign sk (tx, logChainHead), logChainHead)
