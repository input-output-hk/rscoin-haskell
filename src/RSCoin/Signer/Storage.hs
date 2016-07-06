{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for mintette's data.

module RSCoin.Signer.Storage
        ( Storage
        , emptySignerStorage
        , getSignatures
        , addSignature
        , announceNewPeriod
        , pollTransactions
        ) where

import           Control.Lens (makeLenses, view, (%=))
import           RSCoin.Signer.Error
import           Data.Acid    (Query, Update)
import           Control.Monad.Catch        (throwM)
import qualified Data.Map     as M
import qualified Data.Set     as S
import Control.Monad (forM_)

import           RSCoin.Core  (Address, Signature, Transaction, HBlock(..), AddressStrategyMap, Utxo, computeOutputAddrids, PeriodId)

type AddressSignatureMap = M.Map Address Signature

data Storage = Storage
    { _txPool :: M.Map Address (M.Map Transaction AddressSignatureMap) -- pool of trasactions to be signed
    , _addresses :: AddressStrategyMap
    , _unspentAddrIds :: M.Map Address (S.Set AddrId)
    , _utxo :: Utxo
    , _txPoolAddrIds :: M.Map AddrId (Address, Transaction)
    , _periodId :: PeriodId
    } deriving Show

$(makeLenses ''Storage)

emptySignerStorage :: Storage
emptySignerStorage = Storage M.empty M.empty M.empty M.empty M.empty (-1)

-- @TODO put data to temporal queue if one of addrids isn't known yet
-- auto-remove this pendings after N periods
addSignature :: Transaction -> Address -> (Address, Signature) -> Update Storage ()
addSignature tx addr sg = do
    checkAddrRelativeToTx
    txPool %= M.alter f k
  where
    k = (tx, addr)
    f Nothing = Just $ uncurry M.singleton sg
    f (Just m) = Just $ uncurry M.insert sg m
    checkAddrRelativeToTx = do
      s <- fromMaybe S.empty . M.lookup addr <$> view unspentAddrIds
      if any (`S.member` s) (txInputs tx)
         then return ()
         else throwM SEAddrNotRelativeToTx

getSignatures :: Transaction -> Address -> Query Storage [(Address, Signature)]
getSignatures tx addr = maybe [] M.assocs . (M.lookup tx <=< M.lookup addr) <$> view txPool

-- @TODO update announceNewPeriod workflow (to send batch)
-- * first bank should acquire signer's actual periodId
-- * then send batch of periods (range)
getPeriod :: Query Storage PeriodId
getPeriod = view periodId

announceNewPeriod :: HBlock -> Update Storage ()
announceNewPeriod HBlock {..} = do
      addresses %= M.union hbAddresses
      forM_ (concatMap txInputs hbTransactions) $ uncurry processTxIn
      forM_ (concatMap computeOutputAddrids hbTransactions) $ uncurry processTxOut
  where
    processTxIn addrId = do
      addrM <- M.lookup addrId <$> view utxo
      when (isJust addrM) $ processTxIn' addrId (fromJust addrM)
    processTxIn' addrId addr = do
        utxo %= M.delete addrId
        unspentAddrIds %= M.alter f addr
      where f Nothing = Nothing
            f (Just s) = f' (S.delete addrId s)
            f' s | S.null s = Nothing
                 | otherwise = Just s
    processTxOut addrId addr = do
        utxo %= M.insert addrId addr
        unspentAddrIds %= M.alter (Just . f) addr
      where f Nothing = S.singleton addrId
            f (Just s) = S.insert addrId s

-- @TODO implement
pollTransactions :: [Address] -> Query Storage [(Address, [(Transaction, [(Address, Signature)])])]
pollTransactions addrs = return []
