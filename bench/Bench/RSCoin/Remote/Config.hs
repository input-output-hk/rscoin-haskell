{-# LANGUAGE TemplateHaskell #-}

-- | Config for remote benchmark.

module Bench.RSCoin.Remote.Config
       ( RemoteConfig (..)
       , ProfilingType (..)
       , BankData (..)
       , MintetteData (..)
       , UsersData (..)
       , readRemoteConfig
       ) where

import qualified Data.Aeson.TH                        as A
import           Data.Text                            (Text)
import           Data.Text.Buildable                  (Buildable (build))
import qualified Data.Yaml                            as Y
import           Formatting                           (bprint, stext, (%))

import           RSCoin.Core                          (Severity)

import           Bench.RSCoin.Remote.StageRestriction (defaultOptions,
                                                       leaveTagOptions)

data RemoteConfig = RemoteConfig
    { rcTransactionsNum :: !Word
    , rcBank            :: !BankData
    , rcMintettes       :: ![MintetteData]
    , rcMintettesNum    :: !(Maybe Word)
    , rcUsers           :: !UsersData
    , rcShardDivider    :: !Word
    , rcShardDelta      :: !Word
    , rcPeriod          :: !Word
    , rcBranch          :: !(Maybe Text)
    } deriving (Show)

data ProfilingType
    = PTStandard      -- ^ '-p'
    | PTDetailed      -- ^ '-P'
    | PTMostDetailed  -- ^ '-pa'
    deriving (Show)

data BankData = BankData
    { bdHasRSCoin :: !Bool
    , bdHost      :: !Text
    , bdProfiling :: !(Maybe ProfilingType)
    , bdBranch    :: !(Maybe Text)
    } deriving (Show)

data MintetteData = MintetteData
    { mdHasRSCoin :: !Bool
    , mdHost      :: !Text
    , mdProfiling :: !(Maybe ProfilingType)
    } deriving (Show)

instance Buildable MintetteData where
    build MintetteData{..} =
        bprint
            ("Mintette `" % stext % "` (" % stext % " rscoin)")
            mdHost
            (if mdHasRSCoin
                 then "has"
                 else "doesn't have")

data UsersData = UsersData
    { udHasRSCoin :: !Bool
    , udHost      :: !Text
    , udProfiling :: !(Maybe ProfilingType)
    , udSeverity  :: !(Maybe Severity)
    , udNumber    :: !Word
    } deriving (Show)

$(A.deriveJSON defaultOptions ''RemoteConfig)
$(A.deriveJSON defaultOptions ''ProfilingType)
$(A.deriveJSON defaultOptions ''BankData)
$(A.deriveJSON defaultOptions ''MintetteData)
$(A.deriveJSON defaultOptions ''UsersData)
$(A.deriveJSON leaveTagOptions ''Severity)

readRemoteConfig :: FilePath -> IO RemoteConfig
readRemoteConfig fp
    = either (error . show) id <$> Y.decodeFileEither fp
