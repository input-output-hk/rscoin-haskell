{-# LANGUAGE TemplateHaskell #-}

-- | Strategy-related data types and functions/helpers.

module RSCoin.Core.Strategy
     ( AddressToTxStrategyMap
     , AllocationAddress  (..)
     , AllocationStrategy (..)
     , MSTxStrategy       (..)
     , TxStrategy         (..)

     -- * 'AllocationAddress' lenses and prisms
     , _Trust
     , _User
     , address

      -- * 'AllocationStrategy' lenses
     , allParties
     , party
     , txStrategy

     -- * 'MSTxStrategy' lenses and isos
     , sigNumber
     , txIso
     , txParties

     -- * Other helpers
     , isStrategyCompleted
     ) where

import           Control.Lens               (Iso', iso, makeLenses, makePrisms)

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
    -- | Strategy of "1 signature per addrid"
    = DefaultStrategy

    -- | Strategy for getting @m@ signatures
    -- out of @length list@, where every signature
    -- should be made by address in list @list@
    | MOfNStrategy Int (Set Address)
    deriving (Read, Show, Eq)

$(deriveSafeCopy 0 'base ''TxStrategy)

-- | Data type only for multisignature address transaction strategies.
data MSTxStrategy = MSTxStrategy
  { _sigNumber :: Int
  , _txParties :: Set Address
  } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''MSTxStrategy)
$(makeLenses ''MSTxStrategy)
$(makePrisms ''MSTxStrategy)

txIso :: Iso' MSTxStrategy TxStrategy
txIso = iso fromTx toMS
  where
    toMS DefaultStrategy = error "isomorphism from Default is not defined"
    toMS (MOfNStrategy m addrs) = MSTxStrategy m addrs

    fromTx MSTxStrategy{..} = MOfNStrategy _sigNumber _txParties

instance Binary MSTxStrategy where
    put MSTxStrategy{..} = do
        put _sigNumber
        put _txParties

    get = MSTxStrategy <$> get <*> get

instance Buildable MSTxStrategy where
    build MSTxStrategy{..} = bprint template _sigNumber (listBuilderJSON _txParties)
      where
        template = "MSTxStrategy {\n" %
                   "  sigNumber: "    % int     % "\n" %
                   "  txParties: "    % F.build % "\n" %
                   "}\n"

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
    = Trust { _address :: Address }  -- ^ PublicKey we believe
    | User  { _address :: Address }  -- ^ PublicKey of other User
    deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''AllocationAddress)
$(makeLenses ''AllocationAddress)
$(makePrisms ''AllocationAddress)

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
-- @TODO: avoid duplication of sets in '_allParties' and '_txStrategy.txParties'
data AllocationStrategy = AllocationStrategy
    { _party      :: AllocationAddress      -- ^ 'AllocationAddress' of current party.
    , _allParties :: Set AllocationAddress  -- ^ 'Set' of all parties for this address.
    , _txStrategy :: MSTxStrategy           -- ^ Transaction strategy after allocation.
    } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''AllocationStrategy)
$(makeLenses ''AllocationStrategy)

instance Binary AllocationStrategy where
    put AllocationStrategy{..} = do
        put _party
        put _allParties
        put _txStrategy

    get = AllocationStrategy <$> get <*> get <*> get

instance Buildable AllocationStrategy where
    build AllocationStrategy{..} = bprint template
        _party
        (listBuilderJSON _allParties)
        _txStrategy
      where
        template = "AllocationStrategy {\n"  %
                   "  party: "      % F.build % "\n" %
                   "  allParties: " % F.build % "\n" %
                   "  txStrategy: " % F.build % "\n" %
                   "}\n"

-- | Checks if the inner state of strategy allows us to send
-- transaction and it will be accepted
isStrategyCompleted :: TxStrategy -> Address -> [(Address, Signature)] -> Transaction -> Bool
isStrategyCompleted DefaultStrategy userAddr signs tx =
    any (\(addr, signature) -> userAddr == addr &&
                         validateSignature signature addr tx) signs
isStrategyCompleted (MOfNStrategy m addresses) _ signs tx =
    let hasSignature userAddr =
            any (\(addr, signature) -> userAddr == addr &&
                                 validateSignature signature addr tx)
                signs
        withSignatures = S.filter hasSignature addresses
    in length withSignatures >= m
