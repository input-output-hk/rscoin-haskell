-- | Command line options for Bank

module BankOptions
       ( Command (..)
       , Options (..)
       , getOptions
       ) where

import qualified Data.Text              as T
import           Options.Applicative    (Parser, auto, command, execParser,
                                         fullDesc, help, helper, info, long,
                                         metavar, option, progDesc, short,
                                         showDefault, subparser, switch, value,
                                         (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationPath,
                                         defaultPeriodDelta,
                                         defaultSecretKeyPath)

data Command
    = Serve
    | AddMintette String Int T.Text
    | AddExplorer String Int T.Text Int
    | RemoveMintette String Int
    | RemoveExplorer String Int

data Options = Options
    { cloCommand       :: Command
    , cloPath          :: FilePath
    , cloPeriodDelta   :: Integer
    , cloLogSeverity   :: Severity
    , cloSkPath        :: FilePath
    , cloAutoCreateKey :: Bool
    , cloConfigPath    :: FilePath
    }

commandParser :: Parser Command
commandParser =
    subparser
        (command
             "serve"
             (info
                  serveOpts
                  (progDesc "Simply run Bank serving users and mintettes")) <>
         command
             "add-mintette"
             (info addMintetteOpts (progDesc "Add given mintette to database")) <>
         command
             "add-explorer"
             (info addExplorerOpts (progDesc "Add given explorer to database")) <>
         command
             "remove-mintette"
             (info
                  removeMintetteOpts
                  (progDesc $
                   "Remove given mintette on the next " <>
                   "period or from pending queue")) <>
         command
             "remove-explorer"
             (info
                  removeExplorerOpts
                  (progDesc "Remove given explorer from bank's database")))
  where
    mHost = strOption (long "host" <> help "Mintette's host" <> metavar "HOST")
    mPort =
        option auto (long "port" <> help "Mintette's port" <> metavar "INT")
    eHost = strOption (long "host" <> help "Explorer's host" <> metavar "HOST")
    ePort =
        option auto (long "port" <> help "Explorer's port" <> metavar "INT")
    serveOpts = pure Serve
    addMintetteOpts =
        AddMintette <$> mHost <*> mPort <*>
        strOption
            (long "key" <> help "Mintette's public key" <>
             metavar "PUBLIC KEY STRING")
    addExplorerOpts =
        AddExplorer <$> eHost <*> ePort <*>
        strOption
            (long "key" <> help "Explorer's public key" <>
             metavar "PUBLIC KEY STRING") <*>
        option
            auto
            (long "id" <> help "Id of period which this explorer expects" <>
             value 0 <>
             showDefault <>
             metavar "INT")
    removeMintetteOpts = RemoveMintette <$> mHost <*> mPort
    removeExplorerOpts = RemoveExplorer <$> eHost <*> ePort

optionsParser :: FilePath -> FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir defaultConfigPath =
    Options <$> commandParser <*>
    strOption
        (long "path" <> value (configDir </> "bank-db") <> showDefault <>
         help "Path to database" <>
         metavar "FILEPATH") <*>
    option
        auto
        (long "period-delta" <> value (toInteger defaultPeriodDelta) <>
         showDefault <>
         help "Period length in seconds" <>
         metavar "INT") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity" <>
         metavar "SEVERITY") <*>
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
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigPath <>
         showDefault <>
         metavar "FILEPATH")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin's Bank")
