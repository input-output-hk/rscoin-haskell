{-# LANGUAGE TemplateHaskell #-}

-- | Strategy-related data types and functions/helpers.

module RSCoin.Core.Strategy
     ( AddressToTxStrategyMap
     , AllocationParty    (..)
     , AllocationStrategy (..)
     , TxStrategy         (..)
     , isStrategyCompleted
     ) where

import           Data.Binary                (Binary (get, put))
import           Data.Map                   (Map)
import           Data.SafeCopy              (base, deriveSafeCopy)
import           Data.Set                   (Set)
import qualified Data.Set                   as S hiding (Set)
import           Data.Text.Buildable        (Buildable (build))

import           Formatting                 (bprint, int, shown, (%))
import qualified Formatting                 as F (build)

import           Serokell.Util.Text         (listBuilderJSON)

import           RSCoin.Core.Crypto.Signing (Signature)
import           RSCoin.Core.Primitives     (Address, Transaction)
import           RSCoin.Core.Transaction    (validateSignature)

-- | Strategy of confirming transactions.
-- Other strategies are possible, like "getting m out of n, but
-- addresses [A,B,C] must sign". Primitive concept is using M/N.
data TxStrategy
    = DefaultStrategy                  -- ^ Strategy of "1 signature per addrid"
    | MOfNStrategy Int (Set Address)   -- ^ Strategy for getting @m@ signatures
                                       -- out of @length list@, where every signature
                                       -- should be made by address in list @list@
    deriving (Read, Show, Eq)

$(deriveSafeCopy 0 'base ''TxStrategy)

instance Binary TxStrategy where
    put DefaultStrategy          = put (0 :: Int, ())
    put (MOfNStrategy m parties) = put (1 :: Int, (m, parties))

    get = do
        (i, payload) <- get
        pure $ case (i :: Int) of
            0 -> DefaultStrategy
            1 -> uncurry MOfNStrategy payload
            _ -> error "unknow binary strategy"

instance Buildable TxStrategy where
    build DefaultStrategy        = "DefaultStrategy"
    build (MOfNStrategy m addrs) = bprint template m (listBuilderJSON addrs)
      where
        template = "TxStrategy {\n" %
                   "  m: "          % int     % "\n" %
                   "  addresses: "  % F.build % "\n" %
                   "}\n"

type AddressToTxStrategyMap = Map Address TxStrategy

-- | This enumeration represents party for SharedStrategyAllocation
data AllocationParty
    = Trusted
    | User
    deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''AllocationParty)

-- | Strategy of multisignature address allocation.
-- Allows to cretate 2 types of MS addresses:
--     1. p-of-q where user address shared some trusted parties
--     2. m-of-n among users
data AllocationStrategy
    = SharedStrategy AllocationParty  -- ^ strategy 1
    | UserStrategy Int (Set Address)  -- ^ strategy 2
    deriving (Show)

$(deriveSafeCopy 0 'base ''AllocationStrategy)

instance Buildable AllocationStrategy where
    build (SharedStrategy party) = bprint ("SharedStrategy : " % shown) party
    build (UserStrategy m addrs) = bprint template m (listBuilderJSON addrs)
      where
        template = "AllocationStrategy {\n"  %
                   "  m: "         % int     % "\n" %
                   "  addresses: " % F.build % "\n" %
                   "}\n"

-- | Checks if the inner state of strategy allows us to send
-- transaction and it will be accepted
isStrategyCompleted :: TxStrategy -> Address -> [(Address, Signature)] -> Transaction -> Bool
isStrategyCompleted DefaultStrategy address signs tx =
    any (\(addr, signature) -> address == addr &&
                         validateSignature signature addr tx) signs
isStrategyCompleted (MOfNStrategy m addresses) _ signs tx =
    let hasSignature address =
            any (\(addr, signature) -> address == addr &&
                                 validateSignature signature addr tx)
                signs
        withSignatures = S.filter hasSignature addresses
    in length withSignatures >= m
