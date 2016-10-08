-- | Command line options for Notary.

module NotaryOptions
        ( Options (..)
        , getOptions
        ) where

import           Data.Text              (Text)
import           Options.Applicative    (Parser, auto, execParser, fullDesc, help, helper,
                                         info, long, many, metavar, option, progDesc,
                                         short, showDefault, switch, value, (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (PeriodId, Severity (Error), configDirectory,
                                         defaultConfigurationPath, defaultSecretKeyPath)
import           RSCoin.Notary.Defaults (defaultAllocationEndurance,
                                         defaultTransactionEndurance)

data Options = Options
    { cliPath           :: FilePath
    , cliLogSeverity    :: Severity
    , cliMemMode        :: Bool
    , cliWebPort        :: Int
    , cliConfigPath     :: FilePath
    , cliTrustedKeys    :: [Text]
    , cliAllocAlive     :: PeriodId
    , cliTxAlive        :: PeriodId
    , cloDefaultContext :: Bool
    , cloRebuildDB      :: Bool
    , cloSkPath         :: FilePath
    , cloAutoCreateKey  :: Bool
    , cloDisableReqs    :: Bool
    } deriving Show

optionsParser :: FilePath -> FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir defaultConfigPath =
    Options <$>
    strOption
        (long "path" <> value (configDir </> "notary-db") <> showDefault <>
         help "Path to Notary database" <>
         metavar "FILEPATH") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity" <>
         metavar "SEVERITY") <*>
    switch (short 'm' <> long "memory-mode" <> help "Run in memory mode") <*>
    option
        auto
        (long "web-port" <> value 8090 <> showDefault <> help "Web port" <>
         metavar "PORT") <*>
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigPath <>
         showDefault <>
         metavar "FILEPATH") <*>
    many
        (strOption $
         long "trust-keys" <> metavar "PUBLIC KEY" <>
         help
             "Public keys notary will trust as master keys. If not specifed \
               \then notary will trust any key") <*>
    option auto
        (long "alloc-alive" <> metavar "INT" <> value defaultAllocationEndurance <>
         showDefault <> help "Number of periods to keep MS allocation requests alive") <*>
    option auto
        (long "tx-alive" <> metavar "INT" <> value defaultTransactionEndurance <>
         showDefault <> help "Number of periods to keep transactions alive") <*>
    switch
        (mconcat
             [ short 'd'
             , long "default-context"
             , help
                   ("Use default NodeContext. " <>
                    "Intended to be used for local deployment")]) <*>
    switch
        (mconcat
             [ short 'r'
             , long "rebuild-db"
             , help
                   ("Erase database if it already exists")]) <*>
    strOption
        (short 'k' <> long "secret-key" <> help "Path to bank secret key" <>
         value defaultSKPath <>
         showDefault <>
         metavar "FILEPATH") <*>
    switch
        (long "auto-create-sk" <>
         help
             ("If the \"sk\" is pointing to non-existing " <>
              "file, generate a keypair")) <*>
    switch
        (long "disable" <>
         help
             ("If set, notary will comminucate with bank only," <>
              " ignoring all other reqests))"))

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin's Notary")
