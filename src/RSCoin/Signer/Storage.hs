{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage for mintette's data.

module RSCoin.Signer.Storage
        ( Storage
        , emptySignerStorage
        , getSignatures
        , addSignature
        ) where

import           Control.Lens              (makeLenses,
                                            view, (%=))

import           Data.Acid                 (Query, Update)
import qualified Data.Map                  as M

import           RSCoin.Core               (Address, Signature, Transaction)

type AddressSignatureMap = M.Map Address Signature

data Storage = Storage
    { _txPool :: M.Map (Transaction, Address) AddressSignatureMap -- pool of trasactions to be signed
    } deriving Show

$(makeLenses ''Storage)

emptySignerStorage :: Storage
emptySignerStorage = Storage M.empty

getSignatures :: Transaction -> Address -> Query Storage [(Address, Signature)]
getSignatures tx addr = maybe [] M.assocs . M.lookup (tx, addr) <$> view txPool

addSignature :: Transaction -> Address -> (Address, Signature) -> Update Storage ()
addSignature tx addr sg = txPool %= M.alter f k
  where k = (tx, addr)
        f Nothing = Just $ uncurry M.singleton sg
        f (Just m) = Just $ uncurry M.insert sg m
