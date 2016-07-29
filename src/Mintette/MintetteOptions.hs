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
                                         defaultConfigurationFileName,
                                         defaultPort, defaultSecretKeyPath)

data Options = Options
    { cloPort          :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    , cloLogSeverity   :: Severity
    , cloMemMode       :: Bool
    , cloConfigPath    :: FilePath
    }

optionsParser :: FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir =
    Options <$>
    option auto (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
    strOption
        (long "path" <> value (configDir </> "mintette-db") <> showDefault <>
         help "Path to database") <*>
    strOption
        (long "sk" <> value defaultSKPath <> metavar "FILEPATH" <> showDefault) <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    switch (short 'm' <> long "memory-mode" <> help "Run in memory mode") <*>
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigurationFileName <>
         showDefault)

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir)
            (fullDesc <> progDesc "RSCoin's Mintette")
