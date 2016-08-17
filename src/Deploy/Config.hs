{-# LANGUAGE TemplateHaskell #-}

-- | Configuration for rscoin-deploy.

module Config
       ( DeployConfig (..)
       , readDeployConfig
       ) where

import qualified Data.Aeson.TH          as A
import qualified Data.Yaml              as Y

import           RSCoin.Core            (Severity)

import           Serokell.Aeson.Options (defaultOptions, leaveTagOptions)

data DeployConfig = DeployConfig
    { dcDirectory        :: !FilePath
    , dcMintettes        :: !Word
    , dcExplorers        :: !Word
    , dcPeriod           :: !Word
    , dcEpoch            :: !Word
    , dcGlobalSeverity   :: !Severity
    , dcBankSeverity     :: !(Maybe Severity)
    , dcNotarySeverity   :: !(Maybe Severity)
    , dcMintetteSeverity :: !(Maybe Severity)
    , dcExplorerSeverity :: !(Maybe Severity)
    } deriving (Show)

$(A.deriveJSON leaveTagOptions ''Severity)
$(A.deriveJSON defaultOptions ''DeployConfig)

readDeployConfig :: FilePath -> IO DeployConfig
readDeployConfig fp =
    either (error . ("[FATAL] Failed to parse config: " ++) . show) id <$>
    Y.decodeFileEither fp
