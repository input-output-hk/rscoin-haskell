{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Explorer data storage.

module RSCoin.Explorer.Storage
       ( Storage
       , mkStorage

       , Query
       , getAddressBalance
       , getAddressTxNumber
       , getAddressTransactions
       , getLastPeriodId
       , getTx

       , Update
       , ExceptUpdate
       , addHBlock

       ) where

import           Control.Lens                      (at, makeLenses, use, view,
                                                    views, (%=), (.=), _Just)
import           Control.Monad                     (unless)
import           Control.Monad.Catch               (MonadThrow (throwM))
import           Control.Monad.Extra               (whenJustM)
import           Control.Monad.Reader              (MonadReader)
import           Control.Monad.State               (MonadState)
import           Data.List                         (foldl', genericDrop,
                                                    genericLength, genericTake)
import qualified Data.Map.Strict                   as M
import           Data.Maybe                        (fromMaybe, isJust)
import           Data.SafeCopy                     (base, deriveSafeCopy)
import           Data.Tuple.Select                 (sel3)

import qualified RSCoin.Core                       as C

import           RSCoin.Explorer.Error             (ExplorerError (..))
import           RSCoin.Explorer.Web.Sockets.Types (AddrId,
                                                    TransactionSummary (..))

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
      _transactionsMap :: M.Map C.TransactionId C.Transaction
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
getTx i = view $ transactionsMap . at i

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
    addEmission _ = pure ()

applyTransaction :: C.Transaction -> ExceptUpdate ()
applyTransaction tx@C.Transaction{..} = do
    transactionsMap . at txHash .= Just tx
    -- FIXME: @akegalj thinks fromJust should be safe here?
    txInputsSummaries <-
        mapM
            (\a ->
                  mkSummaryAddrId a =<< inputToAddr a)
            txInputs
    let txSummary = mkTransactionSummary txInputsSummaries
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
                    M.insertWith (+) (C.getColor c) c m)
              M.empty
              txInputs
        , txsInputsTotal = sum $ map (C.getCoin . sel3) txInputs
        , txsOutputsSum = foldl'
              (\m (_,c) ->
                    M.insertWith (+) (C.getColor c) c m)
              M.empty
              txOutputs
        , txsOutputsTotal = sum $ map (C.getCoin . snd) txOutputs
        }
    -- TODO: use getTx that is already defined in this module
    -- We have to promote Query to Update
    inputToAddr
        :: C.AddrId -> Update (Maybe C.Address)
    inputToAddr (txId,idx,_) =
        fmap (fst . (!! idx) . C.txOutputs) <$>
        (use $ transactionsMap . at txId)
    mkSummaryAddrId :: C.AddrId -> (Maybe C.Address) -> ExceptUpdate AddrId
    mkSummaryAddrId (txId,ind,c) addr
      | isJust addr = return (txId, ind, c, addr)
      | otherwise = do
          hasEmission <- elem txId <$> use emissionHashes
          if hasEmission
              then return (txId, ind, c, Nothing)
              else throwM $ EEInternalError "Invalid transaction id seen."

applyTxInput :: TransactionSummary -> C.AddrId -> Update ()
applyTxInput tx (oldTxId,idx,c) =
    whenJustM (use $ transactionsMap . at oldTxId) applyTxInputDo
  where
    applyTxInputDo oldTx = do
        let addr = fst $ C.txOutputs oldTx !! idx
        changeAddressData tx (-c) addr

applyTxOutput :: TransactionSummary -> (C.Address, C.Coin) -> Update ()
applyTxOutput tx (addr,c) = changeAddressData tx c addr

changeAddressData :: TransactionSummary -> C.Coin -> C.Address -> Update ()
changeAddressData tx c addr = do
    ensureAddressExists addr
    addresses . at addr . _Just . adTransactions %= (tx :)
    addresses . at addr . _Just . adBalance %=
        M.insertWith (+) (C.getColor c) c

ensureAddressExists :: C.Address -> Update ()
ensureAddressExists addr =
    addresses %= M.alter (Just . fromMaybe mkAddressData) addr
