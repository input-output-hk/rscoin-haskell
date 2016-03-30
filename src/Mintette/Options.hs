-- | Command line options for Mintette

module Options
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative (Parser, auto, execParser, fullDesc, help,
                                      helper, info, long, metavar, option,
                                      progDesc, short, showDefault, strOption,
                                      value, (<>))

import           RSCoin.Core         (Severity (Info), defaultPort,
                                      defaultSecretKeyPath)

data Options = Options
    { cloPort          :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    , cloLogSeverity   :: Severity
    }

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options <$>
    option auto (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
    strOption
        (long "path" <> value "mintette-db" <> showDefault <>
         help "Path to database") <*>
    strOption
        (long "sk" <> value defaultSKPath <> metavar "FILEPATH" <> showDefault) <*>
    option auto (long "log-severity" <> value Info <> showDefault)

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Mintette")
