-- | Command line options for Mintette

module Options
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative (Parser, auto, execParser, fullDesc,
                                      helper, info, long, metavar, option,
                                      progDesc, short, strOption, value, (<>))

import           RSCoin.Core         (defaultPort, defaultSecretKeyPath)

data Options = Options
    { cloPort          :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    }

optionsParser :: Parser Options
optionsParser =
    Options <$> option auto (short 'p' <> long "port" <> value defaultPort) <*>
    strOption (long "path" <> value "db") <*>
    strOption (long "sk" <> value defaultSecretKeyPath <> metavar "FILEPATH")

getOptions :: IO Options
getOptions =
    execParser $
    info (helper <*> optionsParser) (fullDesc <> progDesc "RSCoin's Bank")
