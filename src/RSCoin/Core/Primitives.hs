{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | The most basic primitives from the paper.

module RSCoin.Core.Primitives
       ( Color
       , Coin (..)
       , Address (..)
       , AddrId
       , Transaction (..)
       , TransactionId
       ) where

import           Data.Binary         (Binary (get, put))
import           Data.Hashable       (Hashable (hashWithSalt))
import           Data.SafeCopy       (base, deriveSafeCopy)
import           Data.Text.Buildable (Buildable (build))
import qualified Data.Text.Format    as F

import           Serokell.Util.Text  (listBuilderJSON, pairBuilder,
                                      tripleBuilder)

import           RSCoin.Core.Crypto  (Hash, PublicKey)

type Color = Int

-- | Coin is the least possible unit of currency.
-- We use very simple model at this point.
data Coin = Coin
    { getColor :: Color
    , getCoin  :: Rational
    } deriving (Show, Eq, Ord)

instance Binary Coin where
    put Coin{..} = put (getCoin, getColor)
    get = uncurry Coin <$> get

instance Hashable Coin where
    hashWithSalt s Coin{..} = hashWithSalt s (getCoin, getColor)

instance Buildable Coin where
    build (Coin c col) = mconcat [build c, " coin(s) of color ", build col]

instance Num Coin where
    (+) (Coin c col) (Coin c' col')
      | col == col' = Coin (c + c') col
      | otherwise = error "Error: sum of coins with different colors!"
    (*) (Coin c col) (Coin c' col')
      | col == col' = Coin (c * c') col
      | otherwise = error "Error: product of coins with different colors!"
    (-) (Coin c col) (Coin c' col')
      | col == col' = Coin (c - c') col
      | otherwise = error "Error: subtraction of coins with different colors!"
    abs (Coin a b) = Coin (abs a) b
    signum (Coin c col) = Coin (signum c) col
    fromInteger c = Coin (fromInteger c) 0

-- | Address can serve as input or output to transactions.
-- It is simply a public key.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Buildable, Binary, Eq, Hashable)

-- | AddrId identifies usage of address as output of transaction.
-- Basically, it is tuple of transaction identifier, index in list of outputs
-- and associated value.
type AddrId = (TransactionId, Int, Coin)

instance Buildable (TransactionId, Int, Coin) where
    build (t,i,c) =
        mconcat
            [ "Addrid { transactionId="
            , build t
            , ", index="
            , build i
            , ", coins="
            , build c
            , " }"]

-- | Transaction represents act of transfering units of currency from
-- set of inputs to set of outputs.
data Transaction = Transaction
    { txInputs  :: ![AddrId]
    , txOutputs :: ![(Address, Coin)]
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
