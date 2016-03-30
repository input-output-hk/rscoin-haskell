-- | Command line options for Mintette

module Options
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative (Parser, auto, execParser, fullDesc, help,
                                      helper, info, long, metavar, option,
                                      progDesc, short, showDefault, strOption,
                                      value, (<>))

import           RSCoin.Core         (defaultPort, defaultSecretKeyPath)

data Options = Options
    { cloPort          :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    }

optionsParser :: Parser Options
optionsParser =
    Options <$>
    option auto (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
    strOption
        (long "path" <> value "db" <> showDefault <> help "Path to database") <*>
    strOption
        (long "sk" <> value defaultSecretKeyPath <> metavar "FILEPATH" <>
         showDefault)

getOptions :: IO Options
getOptions =
    execParser $
    info (helper <*> optionsParser) (fullDesc <> progDesc "RSCoin's Bank")
