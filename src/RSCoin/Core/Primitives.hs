-- | The most basic primitives from the paper.

module RSCoin.Core.Primitives
       ( Coin (..)
       , Address (..)
       , AddrId
       , Transaction (..)
       , TransactionId
       ) where

import           Data.Binary         (Binary (put, get))
import           Data.Text.Buildable (Buildable (build))
import qualified Data.Text.Format    as F

import           Serokell.Util.Text  (listBuilderJSON, pairBuilder)

import           RSCoin.Core.Crypto  (Hash, PublicKey)

-- | Coin is the least possible unit of currency.
-- We use very simple model at this point.
newtype Coin = Coin
    { getCoin :: Word
    } deriving (Show, Binary)

instance Buildable Coin where
    build (Coin c) = mconcat [build c, " coin(s)"]

-- | Address can serve as input or output to transactions.
-- It is simply a public key.
newtype Address = Address
    { getAddress :: PublicKey
    } deriving (Show, Buildable, Binary)

-- | AddrId identifies usage of address as output of transaction.
-- Basically, it's pair of transaction identifier and index in list of outputs.
type AddrId = (TransactionId, Int)

-- | Transaction represents act of transfering units of currency from
-- set of inputs to set of outputs.
data Transaction = Transaction
    { txInputs  :: [AddrId]
    , txOutputs :: [(Address, Coin)]
    } deriving (Show)

instance Binary Transaction where
    put Transaction{..} = put (txInputs, txOutputs)
    get = uncurry Transaction <$> get

instance Buildable Transaction where
    build Transaction{..} =
        F.build
            template
            ( listBuilderJSON $ map pairBuilder txInputs
            , listBuilderJSON $ map pairBuilder txOutputs)
      where
        template = "Transaction { inputs = {}, outputs = {} }"

-- | Transaction is identified by its hash
type TransactionId = Hash
