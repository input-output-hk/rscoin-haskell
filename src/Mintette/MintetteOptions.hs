-- | Command line options for Mintette

module MintetteOptions
       ( Command (..)
       , Options (..)
       , ServeOptions (..)

       , getOptions
       ) where

import           Options.Applicative    (Parser, auto, command, execParser,
                                         fullDesc, help, helper, info, long,
                                         metavar, option, progDesc, short,
                                         showDefault, subparser, switch, value,
                                         (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationPath,
                                         defaultEpochDelta, defaultPort,
                                         defaultSecretKeyPath)

data Command
    = Serve ServeOptions
    | DumpStatistics

data ServeOptions = ServeOptions
    { cloPort            :: Int
    , cloEpochDelta      :: Integer
    , cloSecretKeyPath   :: FilePath
    , cloAutoCreateKey   :: Bool
    , cloActionLogsLimit :: Word
    }

data Options = Options
    { cloCommand        :: Command
    , cloPath           :: FilePath
    , cloLogSeverity    :: Severity
    , cloMemMode        :: Bool
    , cloConfigPath     :: FilePath
    , cloDefaultContext :: Bool
    , cloRebuildDB      :: Bool
    }

commandParser :: FilePath -> Parser Command
commandParser defaultSKPath =
    subparser
        (command "serve" (info serveOpts (progDesc "Serve users and others")) <>
         command
             "dump-statistics"
             (info (pure DumpStatistics) (progDesc "Dump statistics")))
  where
    serveOpts =
        fmap Serve $
        ServeOptions <$>
        option
            auto
            (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
        option
            auto
            (long "epoch-delta" <> value (toInteger defaultEpochDelta) <>
             showDefault <>
             help "Epoch length in seconds" <>
             metavar "INT") <*>
        strOption
            (long "sk" <> value defaultSKPath <> metavar "FILEPATH" <>
             help "Path to the secret key" <>
             showDefault <>
             metavar "FILEPATH") <*>
        switch
            (long "auto-create-sk" <>
             help
                 ("If the \"sk\" is pointing to non-existing " <>
                  "file, generate a keypair")) <*>
        option auto (long "action-logs-limit" <> value 100000 <> showDefault)

optionsParser :: FilePath -> FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir defaultConfigPath =
    Options <$> commandParser defaultSKPath <*>
    strOption
        (long "path" <> value (configDir </> "mintette-db") <> showDefault <>
         help "Path to database" <>
         metavar "FILEPATH") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity" <>
         metavar "SEVERITY") <*>
    switch (short 'm' <> long "memory-mode" <> help "Run in memory mode") <*>
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigPath <>
         showDefault <>
         metavar "FILEPATH") <*>
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
                   ("Erase database if it already exists")])

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin's Mintette")
