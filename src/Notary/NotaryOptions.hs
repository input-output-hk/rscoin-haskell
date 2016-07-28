-- | Command line options for Notary.

module NotaryOptions
        ( Options (..)
        , getOptions
        ) where

import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, option,
                                         progDesc, short, showDefault, switch,
                                         value, (<>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error),
                                         defaultConfigurationFileName,
                                         defaultSecretKeyPath)

data Options = Options
    { cliPath        :: FilePath
    , cliLogSeverity :: Severity
    , cliMemMode     :: Bool
    , cliConfigPath  :: FilePath
    } deriving Show

optionsParser :: FilePath -> Parser Options
optionsParser _ =
    Options <$>
    strOption
        (long "path" <> value "notary-db" <> showDefault <>
         help "Path to Notary database") <*>
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
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Notary")
