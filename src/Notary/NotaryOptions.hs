-- | Command line options for Notary.

module NotaryOptions
        ( Options (..)
        , getOptions
        ) where

import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, option,
                                         progDesc, short, showDefault, switch,
                                         value, (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationFileName)

data Options = Options
    { cliPath        :: FilePath
    , cliLogSeverity :: Severity
    , cliMemMode     :: Bool
    , cliWebPort     :: Int
    , cliConfigPath  :: FilePath
    } deriving Show

optionsParser :: FilePath -> Parser Options
optionsParser configDir =
    Options <$>
    strOption
        (long "path" <> value (configDir </> "notary-db") <> showDefault <>
         help "Path to Notary database") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    switch (short 'm' <> long "memory-mode" <> help "Run in memory mode") <*>
    option
        auto
        (long "web-port" <> value 8090 <> showDefault <>
         help "Web port") <*>
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigurationFileName <>
         showDefault)

getOptions :: IO Options
getOptions = do
    configDir <- configDirectory
    execParser $
        info
            (helper <*> optionsParser configDir)
            (fullDesc <> progDesc "RSCoin's Notary")
