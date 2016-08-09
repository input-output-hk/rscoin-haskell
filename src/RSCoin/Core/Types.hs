{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | More complex types from the paper.

module RSCoin.Core.Types
       ( PeriodId
       , Mintette (..)
       , Mintettes
       , MintetteId
       , Explorer (..)
       , Explorers
       , ActionLogHead
       , ActionLogHeads
       , CheckConfirmation (..)
       , CheckConfirmations
       , CommitAcknowledgment (..)
       , ActionLogEntry (..)
       , ActionLog
       , ColdKeysSet
       , LBlock (..)
       , PeriodResult
       , Dpk
       , Utxo
       , Pset
       , HBlock (..)
       , NewPeriodData (..)
       , formatNewPeriodData
       ) where

import           Control.Arrow          (first)
import           Data.Binary            (Binary (get, put), Get, Put)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust, isJust)
import           Data.SafeCopy          (base, deriveSafeCopy)
import qualified Data.Set           as S
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F
import           Data.Text.Lazy.Builder (Builder)
import           Data.Word              (Word8)
import           Formatting             (bprint, int, string, (%))
import qualified Formatting

import           Serokell.Util.Text     (listBuilderJSON, listBuilderJSONIndent,
                                         mapBuilder, pairBuilder, tripleBuilder)

import           RSCoin.Core.Crypto     (Hash, PublicKey, Signature)
import           RSCoin.Core.Primitives (AddrId, Address, Transaction)
import           RSCoin.Core.Strategy   (AddressToTxStrategyMap)

-- | Periods are indexed by sequence of numbers starting from 0.
type PeriodId = Int

-- | All the information about a particular mintette.
data Mintette = Mintette
    { mintetteHost :: !String
    , mintettePort :: !Int
    } deriving (Show, Eq, Ord)

instance Binary Mintette where
    put Mintette {..} = do
        put mintetteHost
        put mintettePort
    get = Mintette <$> get <*> get

$(deriveSafeCopy 0 'base ''Mintette)

instance Buildable Mintette where
    build Mintette{..} = F.build template (mintetteHost, mintettePort)
      where
        template = "Mintette ({}:{})"

-- | Mintettes list is stored by Bank and doesn't change over period.
type Mintettes = [Mintette]

instance Buildable Mintettes where
    build = listBuilderJSON

-- | Mintette is identified by it's index in mintettes list stored by Bank.
-- This id doesn't change over period, but may change between periods.
type MintetteId = Int

-- | All the information about a particular block explorer.
data Explorer = Explorer
    { explorerHost :: !String
    , explorerPort :: !Int
    , explorerKey  :: !PublicKey
    } deriving (Show,Eq,Ord)

instance Binary Explorer where
    put Explorer {..} = do
        put explorerHost
        put explorerPort
        put explorerKey
    get = Explorer <$> get <*> get <*> get

$(deriveSafeCopy 0 'base ''Explorer)

instance Buildable Explorer where
    build Explorer{..} =
        bprint
            ("Explorer (" % string % ":" % int % ", pk: " % Formatting.build %
             ")")
            explorerHost
            explorerPort
            explorerKey

-- | List of explorers is stored by bank.
type Explorers = [Explorer]

-- | Each mintette has a log of actions along with hash which is chained.
-- Head of this log is represented by pair of hash and sequence number.
type ActionLogHead = (Hash, Int)

instance Buildable ActionLogHead where
    build = pairBuilder

-- | ActionLogHeads is a map containing head for each mintette with whom
-- the particular mintette has indirectly interacted.
type ActionLogHeads = M.Map PublicKey ActionLogHead

-- | CheckConfirmation is a confirmation received by user from mintette as
-- a result of CheckNotDoubleSpent action.
data CheckConfirmation = CheckConfirmation
    { ccMintetteKey       :: !PublicKey      -- ^ key of corresponding mintette
    , ccMintetteSignature :: !Signature      -- ^ signature for (tx, addrid, head)
    , ccHead              :: !ActionLogHead  -- ^ head of log
    , ccPeriodId          :: !PeriodId       -- ^ period id when confirmation was made
    } deriving (Show, Eq)

instance Binary CheckConfirmation where
    put CheckConfirmation{..} = do
        put ccMintetteKey
        put ccMintetteSignature
        put ccHead
        put ccPeriodId
    get = CheckConfirmation <$> get <*> get <*> get <*> get

instance Buildable CheckConfirmation where
    build CheckConfirmation{..} =
        F.build template (ccMintetteKey, ccMintetteSignature, ccHead)
      where
        template = "CheckConfirmation (key = {}, sugnature = {}, head = {})"

$(deriveSafeCopy 0 'base ''CheckConfirmation)

-- | CheckConfirmations is a bundle of evidence collected by user and
-- sent to mintette as payload for Commit action.
type CheckConfirmations = M.Map (MintetteId, AddrId) CheckConfirmation

instance Buildable CheckConfirmations where
    build = mapBuilder . map (first pairBuilder) . M.assocs

-- | CommitAcknowledgment is sent by mintette to user as an evidence
-- that mintette has included it into lower-level block.
data CommitAcknowledgment = CommitAcknowledgment
    { caMintetteKey       :: !PublicKey      -- ^ key of corresponding mintette
    , caMintetteSignature :: !Signature      -- ^ signature for (tx, logHead)
    , caHead              :: !ActionLogHead  -- ^ head of log
    } deriving (Show, Eq)

instance Binary CommitAcknowledgment where
    put CommitAcknowledgment{..} = do
        put caMintetteKey
        put caMintetteSignature
        put caHead
    get = CommitAcknowledgment <$> get <*> get <*> get

$(deriveSafeCopy 0 'base ''CommitAcknowledgment)

-- | Each mintette mantains a high-integrity action log, consisting of entries.
data ActionLogEntry
    = QueryEntry !Transaction
    | CommitEntry !Transaction
                  !CheckConfirmations
    | CloseEpochEntry !ActionLogHeads
    deriving (Show, Eq)

putByte :: Word8 -> Put
putByte = put

instance Binary ActionLogEntry where
    put (QueryEntry tr) = putByte 0 >> put tr
    put (CommitEntry tr cc) = putByte 1 >> put (tr, cc)
    put (CloseEpochEntry heads) = putByte 2 >> put heads
    get = do
        t <- get :: Get Word8
        case t of
            0 -> QueryEntry <$> get
            1 -> uncurry CommitEntry <$> get
            2 -> CloseEpochEntry <$> get
            _ -> fail "Unexpected ActionLogEntry type"

instance Buildable ActionLogEntry where
    build (QueryEntry tx) = F.build "Query ({})" $ F.Only tx
    build (CommitEntry tx cc) =
        F.build templateCommit
        ( tx
        , cc)
      where
        templateCommit = "Commit (tx = {}, confirmations = {})"
    build (CloseEpochEntry heads) =
        F.build templateClose $ F.Only $ mapBuilder $ M.assocs heads
      where
        templateClose = "CloseEpoch (heads = {})"

$(deriveSafeCopy 0 'base ''ActionLogEntry)

-- | Action log is a list of entries.
type ActionLog = [(ActionLogEntry, Hash)]

instance Buildable ActionLog where
    build = listBuilderJSONIndent 2 . map pairBuilder

-- | Lower-level block generated by mintette in the end of an epoch.
-- To form a lower-level block a mintette uses the transaction set it
-- formed throughout the epoch and the hashes it has received from other
-- mintettes.
data LBlock = LBlock
    { lbHash         :: !Hash            -- ^ hash of
                                         -- (h^(i-1)_bank, h^m_(j-1), hashes, transactions)
    , lbTransactions :: ![Transaction]   -- ^ txset
    , lbSignature    :: !Signature       -- ^ signature given by mintette for hash
    , lbHeads        :: !ActionLogHeads  -- ^ heads received from other mintettes
    } deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''LBlock)

instance Buildable LBlock where
    build LBlock{..} =
        F.build
            template
            ( lbHash
            , listBuilderJSON lbTransactions
            , lbSignature
            , mapBuilder $ M.assocs lbHeads)
      where
        template =
            mconcat
                [ "LBlock {\n"
                , "  hash: {}\n"
                , "  transactions: {}\n"
                , "  signature: {}\n"
                , "  heads: {}\n"
                , "}\n"
                ]


-- | PeriodResult is sent by mintette to bank when period finishes.
type PeriodResult = (PeriodId, [LBlock], ActionLog)

-- | DPK is a list of signatures which authorizies mintettes for one period
type Dpk = [(PublicKey, Signature)]

-- | Utxo is a type used by mintettes. (addrid -> addr) ∈ utxo means
-- that there was an act of money transfer to address, but since then
-- it wasn't used.
type Utxo = M.Map AddrId Address

-- | Pset is a type used by mintettes. (addrid -> transaction) ∈ pset
-- means that mintette confirmed this transaction isn't double-spent
-- for the given period.
type Pset = M.Map AddrId Transaction

instance Buildable Dpk where
    build = listBuilderJSON . map pairBuilder

-- | The set from triples (m,s,sig) where m is master
-- key, s is slave key and sig is signature of s by m.
type ColdKeysSet = S.Set (Address, Address, Signature)

-- | Higher-level block generated by bank in the end of a period.
-- To form a higher-level block bank uses lower-level blocks received
-- from mintettes and simply merges them after checking validity.
data HBlock = HBlock
    { hbHash         :: !Hash
    , hbTransactions :: ![Transaction]
    , hbSignature    :: !Signature
    , hbDpk          :: !Dpk
    , hbAddresses    :: !AddressToTxStrategyMap
    , hbColdKeys     :: !ColdKeysSet
    } deriving (Show, Eq)

$(deriveSafeCopy 0 'base ''HBlock)

instance Binary HBlock where
    put HBlock{..} = do
        put hbHash
        put hbTransactions
        put hbSignature
        put hbDpk
        put hbAddresses
        put hbColdKeys
    get = HBlock <$> get <*> get <*> get <*> get <*> get <*> get

instance Buildable (Address, Address, Signature) where
    build = tripleBuilder

instance Buildable HBlock where
    build HBlock{..} =
        F.build
            template
            ( hbHash
            , listBuilderJSON hbTransactions
            , hbSignature
            , hbDpk
            , listBuilderJSON hbAddresses
            , listBuilderJSON hbColdKeys)
      where
        template =
            mconcat
                [ "HBlock {\n"
                , "  hash: {}\n"
                , "  transactions: {}\n"
                , "  signature: {}\n"
                , "  dpk: {}\n"
                , "  addresses: {}\n"
                , "  cold keys: {}\n"
                , "}\n"]
instance Buildable [HBlock] where
  build = listBuilderJSON

type NewMintetteIdPayload = (MintetteId, Utxo, AddressToTxStrategyMap)

-- | Data sent by server on new period start. If mintette id changes,
-- bank *must* include npdNewIdPayload.
data NewPeriodData = NewPeriodData
    { npdPeriodId     :: !PeriodId                   -- ^ Id of a new period
    , npdMintettes    :: !Mintettes                  -- ^ Mintettes list
    , npdHBlock       :: !HBlock                     -- ^ Last processed HBlock (needed to
                                                     -- update local mintette's utxo)
    , npdNewIdPayload :: !(Maybe NewMintetteIdPayload) -- ^ Data needed for mintette to
                                                     -- restore state if it's Id changes
    , npdDpk          :: !Dpk                        -- ^ Dpk
    } deriving (Show, Eq)

instance Buildable (AddrId, Address) where
    build = pairBuilder

instance Buildable (AddrId, Transaction) where
    build = pairBuilder

instance Buildable Utxo where
    build mapping = listBuilderJSON $ M.toList mapping

instance Buildable Pset where
    build mapping = listBuilderJSON $ M.toList mapping

instance Buildable (Address, Signature) where
    build = pairBuilder
instance Buildable [(Address, Signature)] where
    build = listBuilderJSONIndent 2

instance Buildable [NewPeriodData] where
    build = listBuilderJSONIndent 2

instance Buildable AddressToTxStrategyMap where
    build = mapBuilder . M.assocs

instance Buildable NewMintetteIdPayload where
    build = tripleBuilder

formatNewPeriodData :: Bool -> NewPeriodData -> Builder
formatNewPeriodData withPayload NewPeriodData{..}
  | withPayload && isJust npdNewIdPayload =
      F.build
          templateWithPayload
          (npdPeriodId, npdMintettes, fromJust npdNewIdPayload, npdHBlock)
  | otherwise =
      F.build templateWithoutPayload (npdPeriodId, npdMintettes, npdHBlock)
  where
    templateWithPayload =
        mconcat
            [ "NewPeriodData {\n"
            , "  periodId: {}\n"
            , "  mintettes: {}\n"
            , "  newIdPayload: {}\n"
            , "  HBlock: {}\n"
            , "}\n"]
    templateWithoutPayload =
        mconcat
            [ "NewPeriodData {\n"
            , "  periodId: {}\n"
            , "  mintettes: {}\n"
            , "  HBlock: {}\n"
            , "}\n"]

instance Buildable NewPeriodData where
    build = formatNewPeriodData True

$(deriveSafeCopy 0 'base ''NewPeriodData)
