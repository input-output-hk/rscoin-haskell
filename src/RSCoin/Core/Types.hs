{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | More complex types from the paper.

module RSCoin.Core.Types
       ( PeriodId
       , Mintette (..)
       , Mintettes
       , MintetteId
       , ActionLogHead
       , ActionLogHeads
       , CheckConfirmation (..)
       , CheckConfirmations
       , CommitConfirmation
       , ActionLogEntry (..)
       , ActionLog
       , LBlock (..)
       , PeriodResult
       , Dpk
       , Utxo
       , Pset
       , HBlock (..)
       , NewPeriodData (..)
       ) where

import           Data.Binary            (Binary (get, put), Get, Put)
import qualified Data.Map               as M
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Data.Text.Buildable    (Buildable (build))
import qualified Data.Text.Format       as F
import           Data.Word              (Word8)

import           Serokell.Util.Text     (listBuilderJSON, listBuilderJSONIndent,
                                         mapBuilder, pairBuilder)

import           RSCoin.Core.Crypto     (Hash, PublicKey, Signature)
import           RSCoin.Core.Primitives (AddrId, Address, Transaction)

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
    build Mintette{..} = F.build template $ (mintetteHost, mintettePort)
      where
        template = "Mintette ({}:{})"

-- | Mintettes list is stored by Bank and doesn't change over period.
type Mintettes = [Mintette]

instance Buildable Mintettes where
    build = listBuilderJSON

-- | Mintette is identified by it's index in mintettes list stored by Bank.
-- This id doesn't change over period, but may change between periods.
type MintetteId = Int

-- | Each mintette has a log of actions along with hash which is chained.
-- Head of this log is represented by pair of hash and sequence number.
type ActionLogHead = (Hash, Int)

instance Buildable ActionLogHead where
    build = pairBuilder

-- | ActionLogHeads is a map containing head for each mintette with whom
-- the particular mintette has indirectly interacted.
type ActionLogHeads = M.Map Mintette ActionLogHead

-- | CheckConfirmation is a confirmation received by user from mintette as
-- a result of CheckNotDoubleSpent action.
data CheckConfirmation = CheckConfirmation
    { ccMintetteKey       :: !PublicKey      -- ^ key of corresponding mintette
    , ccMintetteSignature :: !Signature      -- ^ signature for (tx, addrid, head)
    , ccHead              :: !ActionLogHead  -- ^ head of log
    } deriving (Show)

instance Binary CheckConfirmation where
    put CheckConfirmation{..} = do
        put ccMintetteKey
        put ccMintetteSignature
        put ccHead
    get = CheckConfirmation <$> get <*> get <*> get

instance Buildable CheckConfirmation where
    build CheckConfirmation{..} =
        F.build template $ (ccMintetteKey, ccMintetteSignature, ccHead)
      where
        template = "CheckConfirmation (key = {}, sugnature = {}, head = {})"

$(deriveSafeCopy 0 'base ''CheckConfirmation)

-- | CheckConfirmations is a bundle of evidence collected by user and
-- sent to mintette as payload for Commit action.
type CheckConfirmations = M.Map (MintetteId, AddrId) CheckConfirmation

instance Buildable CheckConfirmations where
    build = mapBuilder . map (\(k, v) -> (pairBuilder k, v)) . M.assocs

-- | CommitConfirmation is sent by mintette to user as an evidence
-- that mintette has included it into lower-level block.
type CommitConfirmation = (PublicKey, Signature, ActionLogHead)

-- | Each mintette mantains a high-integrity action log, consisting of entries.
data ActionLogEntry
    = QueryEntry !Transaction
    | CommitEntry !Transaction
                  !CheckConfirmations
    | CloseEpochEntry !ActionLogHeads
    deriving (Show)

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
        F.build templateCommit $
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
    { lbHash         :: !Hash           -- ^ hash of
                                        -- (h^(i-1)_bank, h^m_(j-1), hashes, transactions)
    , lbTransactions :: [Transaction]   -- ^ txset
    , lbSignature    :: !Signature      -- ^ signature given by mintette for hash
    , lbHeads        :: ActionLogHeads  -- ^ heads received from other mintettes
    } deriving (Show)

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

-- | Higher-level block generated by bank in the end of a period.
-- To form a higher-level block bank uses lower-level blocks received
-- from mintettes and simply merges them after checking validity.
data HBlock = HBlock
    { hbHash         :: !Hash
    , hbTransactions :: [Transaction]
    , hbSignature    :: !Signature
    , hbDpk          :: Dpk
    } deriving (Show)

$(deriveSafeCopy 0 'base ''HBlock)

instance Buildable HBlock where
    build HBlock{..} =
        F.build
            template
            (hbHash, listBuilderJSON hbTransactions, hbSignature, hbDpk)
      where
        template =
            mconcat
                [ "HBlock {\n"
                , "  hash: {}\n"
                , "  transactions: {}\n"
                , "  signature: {}\n"
                , "  dpk: {}\n"
                , "}\n"
                ]

-- | Data sent by server on new period start. If mintette id changes,
-- bank *must* include npdNewIdPayload.
data NewPeriodData = NewPeriodData
    { npdPeriodId     :: PeriodId                 -- ^ Id of a new period
    , npdMintettes    :: Mintettes                -- ^ Mintettes list
    , npdHBlock       :: HBlock                   -- ^ Last processed HBlock (needed to
                                                  -- update local mintette's utxo)
    , npdNewIdPayload :: Maybe (MintetteId, Utxo) -- ^ Data needed for mintette to
                                                  -- restore state if it's Id changes
    , npdDpk          :: Dpk                      -- ^ Dpk
    } deriving (Show)

instance Buildable (AddrId, Address) where
    build = pairBuilder

instance Buildable (AddrId, Transaction) where
    build = pairBuilder

instance Buildable Utxo where
    build mapping = listBuilderJSON $ M.toList mapping

instance Buildable Pset where
    build mapping = listBuilderJSON $ M.toList mapping

instance Buildable [NewPeriodData] where
    build = listBuilderJSONIndent 2

instance Buildable (MintetteId, Utxo) where
    build = pairBuilder

instance Buildable NewPeriodData where
    build NewPeriodData{..} =
        F.build template (npdPeriodId, listBuilderJSON npdMintettes, npdNewIdPayload, npdHBlock)
      where
        template =
            mconcat
                [ "NewPeriodData {\n"
                , "  periodId: {}\n"
                , "  mintettes: {}\n"
                , "  newIdPayload: {}\n"
                , "  HBlock: {}\n"
                , "}\n"
                ]

$(deriveSafeCopy 0 'base ''NewPeriodData)
