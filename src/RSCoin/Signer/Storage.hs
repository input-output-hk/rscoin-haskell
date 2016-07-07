{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for mintette's data.

module RSCoin.Signer.Storage
        ( Storage
        , emptySignerStorage
        , getSignatures
        , acquireSignatures
        , addSignature
        , announceNewPeriods
        , getPeriodId
        , pollTransactions
        ) where

import           Control.Exception   (throw)
import           Control.Lens        (makeLenses, use, view, (%=), (.=))
import           Control.Monad       (forM_, when, (<=<))
import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Acid           (Query, Update, liftQuery)
import qualified Data.Foldable       as F
import qualified Data.Map            as M
import           Data.Maybe          (fromJust, fromMaybe, isJust)
import qualified Data.Set            as S
import           RSCoin.Signer.Error

import           RSCoin.Core         (AddrId, Address, AddressStrategyMap,
                                      HBlock (..), PeriodId, Signature,
                                      Strategy (..), Transaction (..), Utxo,
                                      computeOutputAddrids, isStrategyCompleted,
                                      validateSignature)

data Storage = Storage
    { _txPool         :: M.Map Address (M.Map Transaction (M.Map Address Signature)) -- pool of trasactions to be signed
    , _addresses      :: AddressStrategyMap
    , _unspentAddrIds :: M.Map Address (S.Set AddrId)
    , _utxo           :: Utxo
    , _txPoolAddrIds  :: M.Map AddrId (S.Set (Address, Transaction))
    , _periodId       :: PeriodId
    } deriving Show

$(makeLenses ''Storage)

emptySignerStorage :: Storage
emptySignerStorage = Storage M.empty M.empty M.empty M.empty M.empty (-1)

forgetAddrTx :: Address -> Transaction -> Update Storage ()
forgetAddrTx addr tx = do
  txPool %= M.update (ifNotEmpty . M.delete tx) addr
  txPoolAddrIds %= \m -> foldr (M.update $ ifNotEmpty . S.delete (addr, tx)) m (txInputs tx)

ifNotEmpty :: Foldable t => t a -> Maybe (t a)
ifNotEmpty s |F.null s = Nothing
             |otherwise = Just s

getStrategy :: Address -> Update Storage Strategy
getStrategy addr = fromMaybe DefaultStrategy . M.lookup addr <$> use addresses


instance MonadThrow (Update s) where
    throwM = throw

addSignature :: Transaction -> Address -> (Address, Signature) -> Update Storage ()
addSignature tx addr sg@(sigAddr, sig) = do
    checkAddrIdsKnown
    checkAddrRelativeToTx
    checkSigRelativeToAddr
    txMap <- fromMaybe M.empty . M.lookup addr <$> use txPool
    when (not $ tx `M.member` txMap) $
      txPoolAddrIds %= updateTxPoolAddrIds
    txPool %= M.insert addr (M.alter (Just. uncurry M.insert sg . fromMaybe M.empty) tx txMap)
  where
    updateTxPoolAddrIds m = foldr (M.alter f) m $ txInputs tx
      where f = Just . S.insert (addr, tx) . fromMaybe S.empty
    -- Throws error if addrid isn't known (with corresponding label)
    -- User should repeat transaction after some timeout
    checkAddrIdsKnown = do
      u <- use utxo
      when (not $ all (`M.member` u) (txInputs tx)) $
         use periodId >>= throwM . SEAddrIdNotInUtxo
    checkAddrRelativeToTx = do
      s <- fromMaybe S.empty . M.lookup addr <$> use unspentAddrIds
      when (not $ any (`S.member` s) (txInputs tx)) $
         throwM SEAddrNotRelativeToTx
    checkSigRelativeToAddr = do
        strategy <- getStrategy addr
        case strategy of
          DefaultStrategy
            -> throwM $ SEStrategyNotSupported "DefaultStrategy"
          MOfNStrategy _ addrs
            -> when (not $ any (sigAddr ==) addrs) $
                 throwM SEUnrelatedSignature
        when (not $ validateSignature sig sigAddr tx) $
          throwM SEInvalidSignature

acquireSignatures :: Transaction -> Address -> Update Storage [(Address, Signature)]
acquireSignatures tx addr = do
    sgs <- liftQuery (getSignatures tx addr)
    strategy <- getStrategy addr
    when (isStrategyCompleted strategy addr sgs tx) $
        forgetAddrTx addr tx
    return sgs

getSignatures :: Transaction -> Address -> Query Storage [(Address, Signature)]
getSignatures tx addr = maybe [] M.assocs . (M.lookup tx <=< M.lookup addr) <$> view txPool

getPeriodId :: Query Storage PeriodId
getPeriodId = view periodId

announceNewPeriods :: PeriodId -- periodId of latest hblock
                   -> [HBlock] -- blocks, head corresponds to the latest block
                   -> Update Storage ()
announceNewPeriods pId' blocks = do
    pId <- use periodId
    mapM_ announceNewPeriod $ reverse $ take (pId' - pId) blocks
    periodId .= pId'

announceNewPeriod :: HBlock -> Update Storage ()
announceNewPeriod HBlock {..} = do
      addresses %= M.union hbAddresses
      forM_ (concatMap txInputs hbTransactions) processTxIn
      forM_ (concatMap computeOutputAddrids hbTransactions) $ uncurry processTxOut
  where
    processTxIn addrId = do
      addrM <- M.lookup addrId <$> use utxo
      when (isJust addrM) $ processTxIn' addrId (fromJust addrM)
    processTxIn' addrId addr = do
        utxo %= M.delete addrId
        unspentAddrIds %= M.update (ifNotEmpty . S.delete addrId) addr
        txAddrPairs <- fromMaybe S.empty . M.lookup addrId <$> use txPoolAddrIds
        txPoolAddrIds %= M.delete addrId
        forM_ txAddrPairs $ uncurry forgetAddrTx
    processTxOut addrId addr = do
        utxo %= M.insert addrId addr
        unspentAddrIds %= M.alter (Just . S.insert addrId . fromMaybe S.empty) addr

-- @TODO implement
pollTransactions :: [Address] -> Query Storage [(Address, [(Transaction, [(Address, Signature)])])]
pollTransactions addrs = return []
