-- | Command line options for Mintette

module MintetteOptions
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         option, progDesc, short, showDefault,
                                         switch, value, (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationPath,
                                         defaultEpochDelta, defaultPort,
                                         defaultSecretKeyPath)

data Options = Options
    { cloPort          :: Int
    , cloPath          :: FilePath
    , cloEpochDelta    :: Integer
    , cloSecretKeyPath :: FilePath
    , cloAutoCreateKey :: Bool
    , cloLogSeverity   :: Severity
    , cloMemMode       :: Bool
    , cloConfigPath    :: FilePath
    }

optionsParser :: FilePath -> FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir defaultConfigPath =
    Options <$>
    option auto (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
    strOption
        (long "path" <> value (configDir </> "mintette-db") <> showDefault <>
         help "Path to database" <>
         metavar "FILEPATH") <*>
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
         metavar "FILEPATH")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin's Mintette")
