{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Explorer data storage.

module RSCoin.Explorer.Storage
       ( Storage
       , mkStorage

       , Query
       , getAddressCoins
       , getLastPeriodId
       , getTx

       , Update
       , ExceptUpdate
       , addHBlock

       ) where

import           Control.Lens          (at, makeLenses, use, view, views, (%=),
                                        (.=))
import           Control.Monad         (unless)
import           Control.Monad.Catch   (MonadThrow (throwM))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as S
import           Data.Tuple.Select     (sel3)

import qualified RSCoin.Core           as C

import           RSCoin.Explorer.Error (ExplorerError (..))

data Storage = Storage
    { _assets         :: M.Map C.Address (S.Set C.AddrId)
    , _lastPeriodId   :: Maybe C.PeriodId
    ,
      -- | Mapping from transaction id to actual transaction with this id.
      _transactionMap :: M.Map C.TransactionId C.Transaction
    } deriving (Show)

$(makeLenses ''Storage)

-- | Make initial (empty) storage.
mkStorage :: Storage
mkStorage =
    Storage
    { _assets = M.empty
    , _lastPeriodId = Nothing
    , _transactionMap = M.empty
    }

type Query a = forall m. MonadReader Storage m => m a

-- | Get amount of coins (as CoinsMap) available from given address.
getAddressCoins :: C.Address -> Query C.CoinsMap
getAddressCoins addr =
    views assets $ accumulateAddrIds . M.findWithDefault S.empty addr
  where
    accumulateAddrIds = C.coinsToMap . map sel3 . S.toList

-- | Get PeriodId of last added HBlock.
getLastPeriodId :: Query (Maybe C.PeriodId)
getLastPeriodId = view lastPeriodId

-- | Get transaction with given id (if it can be found).
getTx :: C.TransactionId -> Query (Maybe C.Transaction)
getTx i = view $ transactionMap . at i

type Update a = forall m. MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Modify storage by applying given higher-level block. Period
-- identifier is required to check that given HBlock is the next after
-- last applied block.
addHBlock :: C.PeriodId -> C.HBlock -> ExceptUpdate ()
addHBlock pId C.HBlock{..} = do
    expectedPid <- maybe 0 succ <$> use lastPeriodId
    unless (expectedPid == pId) $
        throwM
            EEPeriodMismatch
            { pmExpectedPeriod = expectedPid
            , pmReceivedPeriod = pId
            }
    mapM_ applyTransaction hbTransactions
    lastPeriodId .= Just pId

applyTransaction :: C.Transaction -> ExceptUpdate ()
applyTransaction tx@C.Transaction{..} = do
    let removeInput addrId = assets %= removeInputPure addrId
        removeInputPure addrId = M.map $ S.delete addrId  -- TODO: its inefficient obviously
        addOutput addrId address = assets %= addOutputPure addrId address
        addOutputPure addrId address =
            M.alter (Just . S.insert addrId . fromMaybe S.empty) address
    transactionMap . at (C.hash tx) .= Just tx
    mapM_ removeInput txInputs
    mapM_ (uncurry addOutput) $ C.computeOutputAddrids tx
