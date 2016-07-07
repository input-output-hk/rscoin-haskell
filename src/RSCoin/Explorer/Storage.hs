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

       , Update
       , ExceptUpdate
       , addHBlock

       ) where

import           Control.Lens          (makeLenses, use, view, views, (%=),
                                        (.=))
import           Control.Monad         (unless)
import           Control.Monad.Catch   (MonadThrow (throwM))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import qualified Data.HashMap.Strict   as HM
import qualified Data.HashSet          as HS
import           Data.Maybe            (fromMaybe)
import           Data.Tuple.Select     (sel3)

import qualified RSCoin.Core           as C

import           RSCoin.Explorer.Error (ExplorerError (..))

data Storage = Storage
    { _assets       :: HM.HashMap C.Address (HS.HashSet C.AddrId)
    , _lastPeriodId :: Maybe C.PeriodId
    } deriving (Show)

$(makeLenses ''Storage)

-- | Make initial (empty) storage.
mkStorage :: Storage
mkStorage =
    Storage
    { _assets = HM.empty
    , _lastPeriodId = Nothing
    }

type Query a = forall m. MonadReader Storage m => m a

-- | Get amount of coins (as CoinsMap) available from given address.
getAddressCoins :: C.Address -> Query C.CoinsMap
getAddressCoins addr =
    views assets $ accumulateAddrIds . HM.lookupDefault HS.empty addr
  where
    accumulateAddrIds = C.coinsToMap . map sel3 . HS.toList

-- | Get PeriodId of last add HBlock.
getLastPeriodId :: Query (Maybe C.PeriodId)
getLastPeriodId = view lastPeriodId

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
        removeInputPure addrId = HM.map $ HS.delete addrId  -- TODO: its inefficient obviously
        addOutput addrId address = assets %= addOutputPure addrId address
        addOutputPure addrId address =
            HM.alter (Just . HS.insert addrId . fromMaybe HS.empty) address
    mapM_ removeInput txInputs
    mapM_ (uncurry addOutput) $ C.computeOutputAddrids tx
