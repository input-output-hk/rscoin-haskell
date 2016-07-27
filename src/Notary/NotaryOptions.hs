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

import           RSCoin.Core            (Severity (Error), defaultSecretKeyPath)

data Options = Options
    { cliPath        :: FilePath
    , cliLogSeverity :: Severity
    , cliMemMode     :: Bool
    , cliWebPort     :: Int
    } deriving Show

optionsParser :: FilePath -> Parser Options
optionsParser _ = -- defaultSKPath
    Options
    <$>
    strOption
        (long "path" <> value "notary-db" <> showDefault <>
         help "Path to Notary database")
    <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity")
    <*>
    switch
        (short 'm' <> long "memory-mode" <> help "Run in memory mode")
    <*>
    option
        auto
        (long "web-port" <> value 8090 <> showDefault <>
         help "Web port")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Notary")
