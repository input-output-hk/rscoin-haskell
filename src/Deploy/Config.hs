{-# LANGUAGE TemplateHaskell #-}

-- | Configuration for rscoin-deploy.

module Config
       ( DeployConfig (..)
       , BankData (..)
       , NotaryData (..)
       , MintetteData (..)
       , ExplorerData (..)
       , readDeployConfig
       ) where

import qualified Data.Aeson.TH          as A
import qualified Data.Yaml              as Y

import           RSCoin.Core            (Severity)

import           Serokell.Aeson.Options (defaultOptions, leaveTagOptions)

data DeployConfig = DeployConfig
    { dcDirectory        :: !FilePath
    , dcBank             :: !BankData
    , dcNotary           :: !NotaryData
    , dcMintettes        :: ![MintetteData]
    , dcExplorers        :: ![ExplorerData]
    , dcPeriod           :: !Word
    , dcGlobalSeverity   :: !Severity
    , dcBankSeverity     :: !(Maybe Severity)
    , dcNotarySeverity   :: !(Maybe Severity)
    , dcMintetteSeverity :: !(Maybe Severity)
    , dcExplorerSeverity :: !(Maybe Severity)
    } deriving (Show)

-- TODO: profiling options are ignored now!

data ProfilingType =
    NotImplemented
    deriving (Show)

data BankData = BankData
    { bdSecret    :: !FilePath
    , bdProfiling :: !(Maybe ProfilingType)
    } deriving (Show)

data NotaryData = NotaryData
    { ndProfiling :: !(Maybe ProfilingType)
    } deriving (Show)

data MintetteData = MintetteData
    { mdProfiling :: !(Maybe ProfilingType)
    } deriving (Show)

data ExplorerData = ExplorerData
    { edProfiling :: !(Maybe ProfilingType)
    } deriving (Show)

$(A.deriveJSON leaveTagOptions ''ProfilingType)
$(A.deriveJSON leaveTagOptions ''Severity)
$(A.deriveJSON defaultOptions ''DeployConfig)
$(A.deriveJSON defaultOptions ''BankData)
$(A.deriveJSON defaultOptions ''MintetteData)
$(A.deriveJSON defaultOptions ''ExplorerData)
$(A.deriveJSON defaultOptions ''NotaryData)

readDeployConfig :: FilePath -> IO DeployConfig
readDeployConfig fp =
    either (error . ("[FATAL] Failed to parse config: " ++) . show) id <$>
    Y.decodeFileEither fp
