{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module RSCoin.Core.Protocol.Types
       ( BankLocalControlRequest (..)
       , checkLocalControlRequest
       , BankMethod (..)
       , DumpMethod (..)
       , MintetteMethod (..)
       , ExplorerMethod (..)
       , RSCoinMethod (..)
       , NotaryMethod (..)
       ) where

import           Data.Text.Buildable    (Buildable (build))
import           Data.Text.Lazy.Builder (fromString)

import           RSCoin.Core.Crypto     (PublicKey, Signature, verify)
import           RSCoin.Core.Types      (Explorer (..), Mintette (..), PeriodId)

-- | Requests used in RSCoin transport layer.
data RSCoinMethod
    = RSCBank     BankMethod
    | RSCExplorer ExplorerMethod
    | RSCMintette MintetteMethod
    | RSCNotary   NotaryMethod
    | RSCDump DumpMethod
    deriving (Show)

-- | A request to control the bank from the bank's host side itself,
-- always supplied with a proof of that (sk sign of tuple of all other args)
data BankLocalControlRequest =
      AddMintette Mintette PublicKey Signature
    | AddExplorer Explorer PeriodId Signature
    | RemoveMintette String Int Signature      -- ^ Host/port
    | RemoveExplorer String Int Signature      -- ^ Host/port
    deriving (Show,Eq)

checkLocalControlRequest :: PublicKey -> BankLocalControlRequest -> Bool
checkLocalControlRequest pk (AddMintette m p s)    = verify pk s (m,p)
checkLocalControlRequest pk (AddExplorer e pid s)  = verify pk s (e,pid)
checkLocalControlRequest pk (RemoveMintette h p s) = verify pk s (h,p)
checkLocalControlRequest pk (RemoveExplorer h p s) = verify pk s (h,p)

-- TODO Maybe make it more pretty (e.g. without signature)
instance Buildable BankLocalControlRequest where
    build = fromString . show

-- | Requests processed by a Bank.
data BankMethod
    = GetMintettes
    | GetExplorers
    | GetAddresses
    | GetBlockchainHeight
    | GetHBlocks
    | GetHBlockEmission
    | FinishPeriod
    | LocalControlRequest -- used for adding/removing mintettes/explorers
    deriving (Show)

-- | Requests processed by Explorer.
data ExplorerMethod
    = EMNewBlock
    | EMGetTransaction
    deriving (Show)

-- | Requests processed by a Mintette.
data MintetteMethod
    = PeriodFinished
    | AnnounceNewPeriod
    | CheckTx
    | CommitTx
    | GetMintettePeriod
    deriving (Show)

-- | Requests for multisign transactions.
data NotaryMethod
    = AnnounceNewPeriodsToNotary
    | AllocateMultisig
    | GetNotaryPeriod
    | GetSignatures
    | PollTransactions
    | PublishTransaction
    | QueryCompleteMS
    | QueryMyAllocMS
    | RemoveCompleteMS
    deriving (Show)

-- | Requests for dumping state.
data DumpMethod
    = GetLogs
    | GetMintetteUtxo
    | GetMintetteBlocks
    | GetMintetteLogs
    deriving (Show)
