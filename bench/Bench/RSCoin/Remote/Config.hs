{-# LANGUAGE TemplateHaskell #-}

-- | Config for remote benchmark.

module Bench.RSCoin.Remote.Config
       ( RemoteConfig (..)
       , ProfilingType (..)
       , BankData (..)
       , MintetteData (..)
       , UsersData (..)
       , UserData (..)
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
    { rcBank         :: !BankData
    , rcMintettes    :: ![MintetteData]
    ,
      -- | Optional field which can be used to run only part of
      -- mintettes.
      rcMintettesNum :: !(Maybe [Word])
    , rcUsers        :: !(Maybe UsersData)
    , rcPeriod       :: !Word
    ,
      -- | By default `master` branch is used. This setting allows to
      -- use different branch by all entities. Branch can also be
      -- specified on per-entity basis (only for bank and user now).
      rcBranch       :: !(Maybe Text)
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
    , bdSeverity  :: !(Maybe Severity)
    , bdBranch    :: !(Maybe Text)
    } deriving (Show)

data MintetteData = MintetteData
    { mdHasRSCoin :: !Bool
    , mdHost      :: !Text
    , mdProfiling :: !(Maybe ProfilingType)
    , mdSeverity  :: !(Maybe Severity)
    } deriving (Show)

instance Buildable MintetteData where
    build MintetteData{..} =
        bprint
            ("Mintette `" % stext % "` (" % stext % " rscoin)")
            mdHost
            (if mdHasRSCoin
                 then "has"
                 else "doesn't have")

data UsersData
    = UDSingle  -- ^ Means that users should be run on a single machine
       { udsData            :: !UserData
       ,
         -- ^ Number of users to run
         udsNumber          :: !Word
       ,
         -- | Number of transactions per user (not total).
         udsTransactionsNum :: !Word}
    | UDMultiple -- ^ Means that each user is run on separate machine
       { udmUsers           :: ![UserData]
       ,
         -- | Optional field which can be used to run only part of
         -- users.
         udmNumber          :: !(Maybe Word)
       , udmTransactionsNum :: !Word
       , udmLogInterval     :: !(Maybe Word)}
    deriving (Show)

data UserData = UserData
    { udHasRSCoin :: !Bool
    , udHost      :: !Text
    , udPrintTPS  :: !(Maybe Bool)
    , udProfiling :: !(Maybe ProfilingType)
    , udSeverity  :: !(Maybe Severity)
    , udBranch    :: !(Maybe Text)
    } deriving (Show)

$(A.deriveJSON leaveTagOptions ''Severity)
$(A.deriveJSON defaultOptions ''RemoteConfig)
$(A.deriveJSON defaultOptions ''ProfilingType)
$(A.deriveJSON defaultOptions ''BankData)
$(A.deriveJSON defaultOptions ''MintetteData)
$(A.deriveJSON defaultOptions ''UsersData)
$(A.deriveJSON defaultOptions ''UserData)

readRemoteConfig :: FilePath -> IO RemoteConfig
readRemoteConfig fp =
    either (error . ("[FATAL] Failed to parse config: " ++) . show) id <$>
    Y.decodeFileEither fp
