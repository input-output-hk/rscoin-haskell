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
import           Data.Ratio          (denominator, numerator)
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
    put Coin{..} = put (getColor, getCoin)
    get = uncurry Coin <$> get

instance Hashable Coin where
    hashWithSalt s Coin{..} = hashWithSalt s (getColor, getCoin)

instance Buildable Coin where
    build (Coin col c) =
        mconcat [ if denominator c == 1
                  then build $ numerator c
                  else build c
                , " coin(s) of color "
                , build col
                ]

reportError :: String -> Coin -> Coin -> a
reportError s c1 c2 =
        error $ "Error: " ++ s ++
                " of coins with different colors: " ++
                show c1 ++ " " ++ show c2

instance Num Coin where
    (+) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c + c')
      | otherwise = reportError "sum" c1 c2
    (*) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c * c')
      | otherwise = reportError "product" c1 c2
    (-) c1@(Coin col c) c2@(Coin col' c')
      | col == col' = Coin col (c - c')
      | otherwise = reportError "subtraction" c1 c2
    abs (Coin a b) = Coin a (abs b)
    signum (Coin col c) = Coin col (signum c)
    fromInteger c = Coin 0 (fromInteger c)

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
