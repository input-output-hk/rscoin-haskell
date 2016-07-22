{-# LANGUAGE TemplateHaskell #-}

-- | Strategy-related data types and functions/helpers.

module RSCoin.Core.Strategy
     ( AddressToTxStrategyMap
     , AllocationAddress  (..)
     , AllocationInfo     (..)
     , AllocationStrategy (..)
     , MSAddress
     , PartyAddress       (..)
     , TxStrategy         (..)

     -- * 'AllocationAddress' lenses and prisms
     , address

      -- * 'AllocationInfo' lenses
     , allocationStrategy
     , currentConfirmations

      -- * 'AllocationStrategy' lenses
     , allParties
     , sigNumber

     -- * Other helpers
     , allocateTxFromAlloc
     , isStrategyCompleted
     , partyToAllocation
     ) where

import           Control.Lens               (makeLenses, traversed, (^..))

import           Data.Binary                (Binary (get, put), getWord8,
                                             putWord8)
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

-- | Type alisas for places where address is used as multisignature address.
type MSAddress = Address

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

instance Binary TxStrategy where
    put DefaultStrategy          = putWord8 0 >> put ()
    put (MOfNStrategy m parties) = putWord8 1 >> put m >> put parties

    get = do
        i <- getWord8
        case i of
            0 -> pure DefaultStrategy
            1 -> MOfNStrategy <$> get <*> get
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

-- | This represents party for AllocationStrategy in set of all participants.
data AllocationAddress
    = TrustAlloc { _address :: Address }  -- ^ PublicKey we trust
    | UserAlloc  { _address :: Address }  -- ^ PublicKey of other User
    deriving (Eq, Ord, Show)

$(deriveSafeCopy 0 'base ''AllocationAddress)
$(makeLenses ''AllocationAddress)

instance Binary AllocationAddress where
    put (TrustAlloc addr) = put (0 :: Int, addr)
    put (UserAlloc  addr) = put (1 :: Int, addr)

    get = do
        (i, addr) <- get
        let ctor = case (i :: Int) of
                       0 -> TrustAlloc
                       1 -> UserAlloc
                       _ -> error "unknown binary AllocationAddress"
        pure $ ctor addr

instance Buildable AllocationAddress where
    build (TrustAlloc addr) = bprint ("TrustA : " % F.build) addr
    build (UserAlloc  addr) = bprint ("UserA  : " % F.build) addr

-- | This datatype represents party who sends request to Notary.
data PartyAddress
    = TrustParty
        { generatedAddress :: Address  -- ^ New generated locally address.
        , publicAddress    :: Address  -- ^ Identified address of 'TrustAlloc'
        }
    | UserParty
        { generatedAddress :: Address  -- ^ Same as for 'TrustParty'
        }
    deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''PartyAddress)

instance Binary PartyAddress where
    put (TrustParty genAddr pubAddr) = putWord8 0 >> put genAddr >> put pubAddr
    put (UserParty  genAddr)         = putWord8 1 >> put genAddr

    get = do
        i <- getWord8
        case i of
            0 -> TrustParty <$> get <*> get
            1 -> UserParty  <$> get
            _ -> error "unknown binary AllocationAddress"

-- @TODO: not so pretty output but ok for now
instance Buildable PartyAddress where
    build (TrustParty genAddr pubAddr) =
        bprint ("TrustP : " % F.build % ", " % F.build) genAddr pubAddr
    build (UserParty  genAddr) = bprint ("UserP  : " % F.build) genAddr

-- | Strategy of multisignature address allocation.
-- @TODO: avoid duplication of sets in '_allParties' and '_txStrategy.txParties'
data AllocationStrategy = AllocationStrategy
    { _sigNumber  :: Int                    -- ^ Number of required signatures in transaction
    , _allParties :: Set AllocationAddress  -- ^ 'Set' of all parties for this address
    } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''AllocationStrategy)
$(makeLenses ''AllocationStrategy)

instance Binary AllocationStrategy where
    put AllocationStrategy{..} = do
        put _sigNumber
        put (S.toList _allParties)

    get = AllocationStrategy <$> get <*> (S.fromList <$> get)

instance Buildable AllocationStrategy where
    build AllocationStrategy{..} = bprint template
        _sigNumber
        (listBuilderJSON _allParties)
      where
        template = "AllocationStrategy {\n"  %
                   "  sigNumber: "  % F.build % "\n" %
                   "  allParties: " % F.build % "\n" %
                   "}\n"

-- | Stores meta information for MS allocation by 'AlocationStrategy'.
data AllocationInfo = AllocationInfo
    { _allocationStrategy   :: AllocationStrategy
    , _currentConfirmations :: Map AllocationAddress Address
    } deriving (Eq, Show)

$(deriveSafeCopy 0 'base ''AllocationInfo)
$(makeLenses ''AllocationInfo)

instance Buildable AllocationInfo where
    build AllocationInfo{..} = bprint template
        _allocationStrategy
        (listBuilderJSON _currentConfirmations)
      where
        template = "AllocationStrategy {\n"   %
                   "  allocationStrategy: "   % F.build % "\n" %
                   "  currentConfirmations: " % F.build % "\n" %
                   "}\n"

-- | Creates corresponding multisignature 'TxStrategy'.
allocateTxFromAlloc :: AllocationStrategy -> TxStrategy
allocateTxFromAlloc AllocationStrategy{..} =
    MOfNStrategy
        _sigNumber $
        S.fromList $ (S.toList _allParties)^..traversed.address

-- | Converts 'PartyAddress' to original 'AllocationAddress'.
partyToAllocation :: PartyAddress -> AllocationAddress
partyToAllocation TrustParty{..} = TrustAlloc publicAddress
partyToAllocation UserParty{..}  = UserAlloc generatedAddress

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
