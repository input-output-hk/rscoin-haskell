{-# LANGUAGE TemplateHaskell #-}

-- | Configuration for rscoin-deploy.

module Config
       ( DeployConfig (..)
       , BankData (..)
       , MintetteData (..)
       , readDeployConfig
       ) where

import qualified Data.Aeson.TH          as A
import           Data.Text              (Text)
import qualified Data.Yaml              as Y
import           Serokell.Aeson.Options (defaultOptions, leaveTagOptions)

import           RSCoin.Core            (Severity)

data DeployConfig = DeployConfig
    { dcDirectory :: !FilePath
    , dcExec      :: !Text
    , dcBank      :: !BankData
    , dcMintettes :: ![MintetteData]
    , dcPeriod    :: !Word
    } deriving (Show)

data BankData = BankData
    { bdPort     :: !(Maybe Int)
    -- , bdProfiling :: !(Maybe ProfilingType)
    , bdSeverity :: !(Maybe Severity)
    } deriving (Show)

data MintetteData = MintetteData
    { mdPort     :: !(Maybe Int)
    -- , mdProfiling :: !(Maybe ProfilingType)
    , mdSeverity :: !(Maybe Severity)
    } deriving (Show)

$(A.deriveJSON leaveTagOptions ''Severity)
$(A.deriveJSON defaultOptions ''DeployConfig)
$(A.deriveJSON defaultOptions ''BankData)
$(A.deriveJSON defaultOptions ''MintetteData)

readDeployConfig :: FilePath -> IO DeployConfig
readDeployConfig fp =
    either (error . ("[FATAL] Failed to parse config: " ++) . show) id <$>
    Y.decodeFileEither fp
