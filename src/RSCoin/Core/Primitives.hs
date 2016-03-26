{-# LANGUAGE TemplateHaskell #-}

-- | The most basic primitives from the paper.

module RSCoin.Core.Primitives
       ( Coin (..)
       , Address (..)
       , AddrId
       , Transaction (..)
       , TransactionId
       ) where

import           Data.Binary         (Binary (get, put))
import           Data.Hashable       (Hashable (hashWithSalt))
import           Data.Int            (Int64)
import           Data.SafeCopy       (base, deriveSafeCopy)
import           Data.Text.Buildable (Buildable (build))
import qualified Data.Text.Format    as F

import           Serokell.Util.Text  (listBuilderJSON, pairBuilder,
                                      tripleBuilder)

import           RSCoin.Core.Crypto  (Hash, PublicKey)

-- | Coin is the least possible unit of currency.
-- We use very simple model at this point.
newtype Coin = Coin
    { getCoin :: Int64
    } deriving (Show, Binary, Num, Eq, Ord, Hashable)

instance Buildable Coin where
    build (Coin c) = mconcat [build c, " coin(s)"]

-- | Address can serve as input or output to transactions.
-- It is simply a public key.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Buildable, Binary, Eq, Hashable)

-- | AddrId identifies usage of address as output of transaction.
-- Basically, it is tuple of transaction identifier, index in list of outputs
-- and associated value.
type AddrId = (TransactionId, Int, Coin)

-- | Transaction represents act of transfering units of currency from
-- set of inputs to set of outputs.
data Transaction = Transaction
    { txInputs  :: [AddrId]
    , txOutputs :: [(Address, Coin)]
    } deriving (Show, Eq)

instance Binary Transaction where
    put Transaction{..} = put (txInputs, txOutputs)
    get = uncurry Transaction <$> get

instance Hashable Transaction where
    hashWithSalt s Transaction{..} = hashWithSalt s (txInputs, txOutputs)

instance Buildable Transaction where
    build Transaction{..} =
        F.build
            template
            ( listBuilderJSON $ map tripleBuilder txInputs
            , listBuilderJSON $ map pairBuilder txOutputs)
      where
        template = "Transaction { inputs = {}, outputs = {} }"

-- | Transaction is identified by its hash
type TransactionId = Hash

$(deriveSafeCopy 0 'base ''Coin)
$(deriveSafeCopy 0 'base ''Address)
$(deriveSafeCopy 0 'base ''Transaction)
