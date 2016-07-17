{-# LANGUAGE TemplateHaskell #-}

-- | Strategy-related data types and functions/helpers.

module RSCoin.Core.Strategy
     ( AddressToTxStrategyMap
     , AllocationAddress  (..)
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

import           Formatting                 (bprint, int, (%))
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

-- | This represents party for AllocationStrategy.
data AllocationAddress
    = Trust Address  -- ^ PublicKey we believe
    | User  Address  -- ^ PublicKey of other User
    deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''AllocationAddress)

instance Binary AllocationAddress where
    put (Trust addr) = put (0 :: Int, addr)
    put (User  addr) = put (1 :: Int, addr)

    get = do
        (i, addr) <- get
        let ctor = case (i :: Int) of
                       0 -> Trust
                       1 -> User
                       _ -> error "unknown binary AllocationAddress"
        pure $ ctor addr

instance Buildable AllocationAddress where
    build (Trust addr) = bprint ("Trust : " % F.build) addr
    build (User  addr) = bprint ("User  : " % F.build) addr

-- | Strategy of multisignature address allocation.
data AllocationStrategy = AllocationStrategy
    { party      :: AllocationAddress      -- ^ 'AllocationAddress' of current party.
    , allParties :: Set AllocationAddress  -- ^ 'Set' of all parties for this address.
    , txStrategy :: TxStrategy             -- ^ Transaction strategy after allocation.
    } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''AllocationStrategy)

instance Binary AllocationStrategy where
    put AllocationStrategy{..} = do
        put party
        put allParties
        put txStrategy

    get = AllocationStrategy <$> get <*> get <*> get

instance Buildable AllocationStrategy where
    build AllocationStrategy{..} = bprint template party (listBuilderJSON allParties) txStrategy
      where
        template = "AllocationStrategy {\n"  %
                   "  party: "      % F.build % "\n" %
                   "  allParties: " % F.build % "\n" %
                   "  txStrategy: " % F.build % "\n" %
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
