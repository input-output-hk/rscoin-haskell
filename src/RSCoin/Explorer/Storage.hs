{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Explorer data storage.

module RSCoin.Explorer.Storage
       ( Storage
       , mkStorage

       , Query
       , addressExists
       , getAddressBalance
       , getAddressTxNumber
       , getAddressTransactions
       , getLastPeriodId
       , getTx
       , getTxSummary

       , Update
       , ExceptUpdate
       , addHBlock

       ) where

import           Control.Lens                       (at, makeLenses, use, view,
                                                     views, (%=), (.=), _3,
                                                     _Just)
import           Control.Monad                      (unless)
import           Control.Monad.Catch                (MonadThrow (throwM))
import           Control.Monad.Extra                (whenJustM)
import           Control.Monad.Reader               (MonadReader)
import           Control.Monad.State                (MonadState)
import qualified Data.IntMap.Strict                 as I
import           Data.List                          (foldl', genericDrop,
                                                     genericLength, genericTake)
import qualified Data.Map.Strict                    as M
import           Data.Maybe                         (fromMaybe, isJust)
import           Data.SafeCopy                      (base, deriveSafeCopy)
import           Formatting                         (build, sformat, (%))

import qualified RSCoin.Core                        as C

import           RSCoin.Explorer.Error              (ExplorerError (..))
import           RSCoin.Explorer.TransactionSummary (ExtendedAddrId,
                                                     TransactionSummary (..),
                                                     txSummaryToTx)

$(deriveSafeCopy 0 'base ''TransactionSummary)

data AddressData = AddressData
    { _adBalance      :: C.CoinsMap
    , _adTransactions :: [TransactionSummary]
    }

mkAddressData :: AddressData
mkAddressData =
    AddressData
    { _adBalance = C.zeroCoinsMap
    , _adTransactions = []
    }

$(makeLenses ''AddressData)

$(deriveSafeCopy 0 'base ''AddressData)

-- TODO: store blocks, store transactions map more efficiently (see
-- `change-transcation-map` for example).
data Storage = Storage
    {
      -- | State of all addresses ever seen by this explorer.
      _addresses       :: M.Map C.Address AddressData
    ,
      -- | PeriodId of last added HBlock.
      _lastPeriodId    :: Maybe C.PeriodId
    ,
      -- | Mapping from transaction id to actual transaction with this
      -- id. Contains all transactions ever seen by this explorer.
      _transactionsMap :: M.Map C.TransactionId TransactionSummary
      -- | List off all emission hashes from the very beginning.
    , _emissionHashes  :: [C.TransactionId]
    }

$(makeLenses ''Storage)

$(deriveSafeCopy 0 'base ''Storage)

-- | Make initial (empty) storage.
mkStorage :: Storage
mkStorage =
    Storage
    { _addresses = M.empty
    , _lastPeriodId = Nothing
    , _transactionsMap = M.empty
    , _emissionHashes = []
    }

type Query a = forall m. MonadReader Storage m => m a

addTimestamp :: Query a -> Query (C.PeriodId, a)
addTimestamp q = (,) <$> (maybe 0 succ <$> getLastPeriodId) <*> q

-- | Get amount of coins (as CoinsMap) available from given
-- address. Result is timestamped with id of ongoing period.
getAddressBalance :: C.Address -> Query (C.PeriodId, C.CoinsMap)
getAddressBalance addr =
    addTimestamp $
    views (addresses . at addr) (maybe C.zeroCoinsMap (view adBalance))

-- | Get number of transactions refering to given address. Result is
-- timestamped with id of ongoing period.
getAddressTxNumber :: C.Address -> Query (C.PeriodId, Word)
getAddressTxNumber addr =
    addTimestamp $
    views (addresses . at addr) (maybe 0 (genericLength . view adTransactions))

-- | Get subset of transactions referring to given address. Index of
-- the most recent transaction is 0. Returns indexed list of
-- transactions in range [lo, min (hi, txNum)). Result is timestamped
-- with id of ongoing period.
getAddressTransactions :: C.Address
                       -> (Word, Word)
                       -> Query (C.PeriodId, [(Word, TransactionSummary)])
getAddressTransactions addr indices =
    addTimestamp $
    indexedSubList indices <$>
    views (addresses . at addr) (maybe [] (view adTransactions))

indexedSubList :: (Word, Word) -> [a] -> [(Word, a)]
indexedSubList (lo,hi)
  | hi <= lo = const []
  | otherwise = zip [lo .. hi - 1] . genericTake (hi - lo) . genericDrop lo

-- | Get PeriodId of last added HBlock.
getLastPeriodId :: Query (Maybe C.PeriodId)
getLastPeriodId = view lastPeriodId

-- | Get transaction with given id (if it can be found).
getTx :: C.TransactionId -> Query (Maybe C.Transaction)
getTx = fmap (fmap txSummaryToTx) . getTxSummary

-- | Get summary of transaction with given id (if it can be found).
getTxSummary :: C.TransactionId -> Query (Maybe TransactionSummary)
getTxSummary i = view $ transactionsMap . at i

-- | Cheks whether address exists in storage
addressExists :: C.Address -> Query Bool
addressExists addr = fmap isJust . view $ addresses . at addr

type Update a = forall m. MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Modify storage by applying given higher-level block. Period
-- identifier is required to check that given HBlock is the next after
-- last applied block.
addHBlock :: C.PeriodId -> C.HBlock -> C.EmissionId -> ExceptUpdate ()
addHBlock pId C.HBlock{..} emission = do
    expectedPid <- maybe 0 succ <$> use lastPeriodId
    unless (expectedPid == pId) $
        throwM
            EEPeriodMismatch
            { pmExpectedPeriod = expectedPid
            , pmReceivedPeriod = pId
            }
    addEmission emission
    mapM_ applyTransaction hbTransactions
    lastPeriodId .= Just pId
  where
    addEmission (Just e) = emissionHashes %= (e:)
    addEmission _        = pure ()

applyTransaction :: C.Transaction -> ExceptUpdate ()
applyTransaction tx@C.Transaction{..} = do
    txInputsSummaries <-
        mapM
            (\a ->
                  mkExtendedAddrId a =<< inputToAddr a)
            txInputs
    let txSummary = mkTransactionSummary txInputsSummaries
    transactionsMap . at txHash .= Just txSummary
    mapM_ (applyTxInput txSummary) txInputs
    mapM_ (applyTxOutput txSummary) txOutputs
  where
    txHash = C.hash tx
    mkTransactionSummary summaryTxInputs =
        TransactionSummary
        { txsId = txHash
        , txsInputs = summaryTxInputs
        , txsOutputs = txOutputs
        , txsInputsSum = foldl'
              (\m (_,_,c) ->
                    I.insertWith (+) (C.getC $ C.getColor c) c m)
              I.empty
              txInputs
        , txsInputsTotal = sum $ map (C.getCoin . view _3) txInputs
        , txsOutputsSum = foldl'
              (\m (_,c) ->
                    I.insertWith (+) (C.getC $ C.getColor c) c m)
              I.empty
              txOutputs
        , txsOutputsTotal = sum $ map (C.getCoin . snd) txOutputs
        }
    -- TODO: use getTx that is already defined in this module
    -- We have to promote Query to Update
    inputToAddr
        :: C.AddrId -> Update (Maybe C.Address)
    inputToAddr (txId,idx,_) =
        fmap (fst . (!! idx) . txsOutputs) <$>
        (use $ transactionsMap . at txId)
    mkExtendedAddrId :: C.AddrId
                     -> (Maybe C.Address)
                     -> ExceptUpdate ExtendedAddrId
    mkExtendedAddrId (txId,ind,c) addr
      | isJust addr = return (txId, ind, c, addr)
      | otherwise = do
          hasEmission <- elem txId <$> use emissionHashes
          if hasEmission
              then return (txId, ind, c, Nothing)
              else throwM $ EEInternalError $
                   sformat ("Invalid transaction id seen: " % build) txId

applyTxInput :: TransactionSummary -> C.AddrId -> Update ()
applyTxInput tx (oldTxId,idx,c) =
    whenJustM (use $ transactionsMap . at oldTxId) applyTxInputDo
  where
    applyTxInputDo oldTx = do
        let addr = fst $ txsOutputs oldTx !! idx
        changeAddressData tx (-c) addr

applyTxOutput :: TransactionSummary -> (C.Address, C.Coin) -> Update ()
applyTxOutput tx (addr,c) = changeAddressData tx c addr

changeAddressData :: TransactionSummary -> C.Coin -> C.Address -> Update ()
changeAddressData tx c addr = do
    ensureAddressExists addr
    addresses . at addr . _Just . adTransactions %= (tx :)
    addresses . at addr . _Just . adBalance %=
        I.insertWith (+) (C.getC $ C.getColor c) c

ensureAddressExists :: C.Address -> Update ()
ensureAddressExists addr =
    addresses %= M.alter (Just . fromMaybe mkAddressData) addr
