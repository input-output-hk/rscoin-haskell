{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Storage for mintette's data.

module RSCoin.Mintette.Storage
       ( Storage
       , mkStorage
       , checkNotDoubleSpent
       , commitTx
       , finishPeriod
       , startPeriod
       , finishEpoch
       ) where

import           Control.Applicative       ((<|>))
import           Control.Lens              (Getter, makeLenses, to, use, uses,
                                            (%=), (+=), (<>=))
import           Control.Monad             (when)
import           Control.Monad.Catch       (MonadThrow (throwM))
import           Control.Monad.State.Class (MonadState)
import qualified Data.Map                  as M
import           Data.Maybe                (fromJust)
import qualified Data.Set                  as S
import           Data.Tuple.Select         (sel1)
import           Safe                      (headMay)

import           RSCoin.Core               (ActionLog, ActionLogEntry (CommitEntry, QueryEntry),
                                            AddrId, Address, CheckConfirmation,
                                            CheckConfirmations,
                                            CommitConfirmation, Hash,
                                            MintetteId, Mintettes,
                                            NewPeriodData, PeriodId,
                                            PeriodResult, SecretKey, Signature,
                                            Transaction (..), actionLogNext,
                                            computeOutputAddrids, hash,
                                            mkCheckConfirmation, owners, sign,
                                            validateSignature, validateSum,
                                            verifyCheckConfirmation)
import           RSCoin.Mintette.Error     (MintetteError (MEInternal))

data Storage = Storage
    { _utxo       :: M.Map AddrId Address      -- ^ Unspent transaction outputs
    , _pset       :: M.Map AddrId Transaction  -- ^ Set of checked transactions
    , _txset      :: S.Set Transaction         -- ^ List of transaction sealing into ledger
    , _actionLogs :: [ActionLog]               -- ^ Logs are stored per period
    , _logSize    :: Int                       -- ^ Total size of actionLogs
    , _mintettes  :: Mintettes                 -- ^ Mintettes for current period
    , _mintetteId :: Maybe MintetteId          -- ^ Id for current period
    }

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage M.empty M.empty S.empty [] 0 [] Nothing

logHead :: Getter Storage (Maybe (ActionLogEntry, Hash))
logHead = actionLogs . to f
  where
    f [] = Nothing
    f (x:xs) = headMay x <|> f xs

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: SecretKey
                    -> Transaction
                    -> AddrId
                    -> Signature
                    -> ExceptUpdate (Maybe CheckConfirmation)
checkNotDoubleSpent sk tx addrId sg = do
    inPset <- M.lookup addrId <$> use pset
    let txContainsAddrId = addrId `elem` txInputs tx
    addr <- M.lookup addrId <$> use utxo
    let isValid =
            maybe
                (and [txContainsAddrId, signatureValid addr, validateSum tx])
                (== tx)
                inPset
    finishCheck isValid
  where
    signatureValid =
        maybe False (\a -> validateSignature sg a tx)
    finishCheck False = return Nothing
    finishCheck True = do
        pushLogEntry $ QueryEntry tx
        utxo %= M.delete addrId
        pset %= M.insert addrId tx
        hsh <- uses logHead (snd . fromJust)
        logSz <- use logSize
        return $ Just $ mkCheckConfirmation sk tx addrId (hsh, logSz - 1)

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.
-- If it's true, add transaction to storage and return signed confirmation.
commitTx :: SecretKey
         -> Transaction
         -> PeriodId
         -> CheckConfirmations
         -> ExceptUpdate (Maybe CommitConfirmation)
commitTx sk tx@Transaction{..} _ bundle = do
    mts <- use mintettes
    mId <- use mintetteId
    let isOwner = maybe False (`elem` owners mts (hash tx)) mId
    let isValid = and [validateSum tx, isOwner]
    let isConfirmed =
            and $
            flip map txInputs $
            \addrid ->
                 let addridOwners = owners mts (hash $ sel1 addrid)
                     ownerConfirmed owner =
                         maybe False
                               -- FIXME There's should be '∈ DPK' check, but I don't see it in state
                               (\proof -> verifyCheckConfirmation proof tx addrid && True) $
                         M.lookup (owner, addrid) bundle
                     filtered = filter ownerConfirmed addridOwners
                 in length filtered > length addridOwners `div` 2
    commitTxChecked (isValid && isConfirmed) sk tx bundle

commitTxChecked
    :: Bool
    -> SecretKey
    -> Transaction
    -> CheckConfirmations
    -> ExceptUpdate (Maybe CommitConfirmation)
commitTxChecked False _ _ _ = return Nothing
commitTxChecked True sk tx bundle = do
    pushLogEntry $ CommitEntry tx bundle
    utxo <>= M.fromList (computeOutputAddrids tx)
    txset %= S.insert tx
    let pk = undefined -- FIXME How do I get it?
    hsh <- uses logHead (snd . fromJust)
    logSz <- use logSize
    let logChainHead = (hsh, logSz)
    return $ Just (pk, sign sk (tx, logChainHead), logChainHead)

-- | Finish ongoing period, returning its result.
-- Do nothing if period id is not an expected one.
finishPeriod :: PeriodId -> Update PeriodResult
finishPeriod = undefined

-- | Start new period.
startPeriod :: NewPeriodData -> Update ()
startPeriod = undefined

-- | Finish current epoch and start a new one.
finishEpoch :: SecretKey -> Update ()
finishEpoch = undefined

pushLogEntry :: ActionLogEntry -> ExceptUpdate ()
pushLogEntry entry = do
    lgs <- use actionLogs
    when (null lgs) $ throwM $ MEInternal "Empty actionLogs"
    let topLog = head lgs
    let newTopLog = actionLogNext (prevHead lgs) entry : topLog
    actionLogs %= (\l -> newTopLog : tail l)
    logSize += 1
  where prevHead [] = Nothing
        prevHead (x:xs) = headMay x <|> prevHead xs
