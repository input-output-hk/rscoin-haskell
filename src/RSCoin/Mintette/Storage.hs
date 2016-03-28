{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Storage for mintette's data.

module RSCoin.Mintette.Storage
       ( Storage
       , mkStorage
       , checkNotDoubleSpent
       , finishPeriod
       , startPeriod
       ) where

import           Control.Applicative       ((<|>))
import           Control.Lens              (makeLenses, use, (%=))
import           Control.Monad             (when)
import           Control.Monad.Catch       (MonadThrow (throwM))
import           Control.Monad.State.Class (MonadState)
import qualified Data.Map                  as M
import           Safe                      (headMay)

import           RSCoin.Core               (ActionLog,
                                            ActionLogEntry (QueryEntry), AddrId,
                                            Address, Dpk, NewPeriodData,
                                            PeriodId, PeriodResult, Signature,
                                            Transaction (txInputs),
                                            actionLogNext, validateSignature,
                                            validateSum)
import           RSCoin.Mintette.Error     (MintetteError (MEInternal))

data Storage = Storage
    { _utxo       :: M.Map AddrId Address
    , _pset       :: M.Map AddrId Transaction
    , _actionLogs :: [ActionLog]
    , _dpk        :: Dpk
    }

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage M.empty M.empty [] []

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: Transaction -> AddrId -> Signature -> ExceptUpdate Bool
checkNotDoubleSpent tx addrid sg = do
    inPset <- M.lookup addrid <$> use pset
    let txContainsAddrId = elem addrid (txInputs tx)
    addr <- M.lookup addrid <$> use utxo
    let res =
            maybe
                (and [txContainsAddrId, signatureValid addr, validateSum tx])
                (== tx)
                inPset
    when res $
        do pushLogEntry $ QueryEntry tx
           utxo %= M.delete addrid
           pset %= M.insert addrid tx
    return res
  where
    signatureValid =
        maybe False (\a -> validateSignature sg a tx)

-- | Finish ongoing period, returning its result.
-- Do nothing if period id is not an expected one.
finishPeriod :: PeriodId -> Update PeriodResult
finishPeriod = undefined

-- | Start new period.
startPeriod :: NewPeriodData -> Update ()
startPeriod = undefined

pushLogEntry :: ActionLogEntry -> ExceptUpdate ()
pushLogEntry entry = do
    lgs <- use actionLogs
    when (null lgs) $ throwM $ MEInternal "Empty actionLogs"
    let topLog = head lgs
    let newTopLog = actionLogNext (prevHead lgs) entry : topLog
    actionLogs %= (\l -> newTopLog : tail l)
  where prevHead [] = Nothing
        prevHead (x:xs) = headMay x <|> prevHead xs
