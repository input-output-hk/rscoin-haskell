-- | Command line options for Bank

module Options
       ( Command (..)
       , Options (..)
       , getOptions
       ) where

import           Options.Applicative (Parser, auto, command, execParser,
                                      fullDesc, help, helper, info, long,
                                      option, progDesc, short, strOption,
                                      subparser, value, (<>))

import           RSCoin.Core         (Mintette (..))

data Command
    = Serve Int
    | AddMintette String Int FilePath

data Options = Options
    { cloCommand :: Command
    , cloPath    :: FilePath
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
             (info addMintetteOpts (progDesc "Add given mintette to database")))
  where
    serveOpts = Serve <$> option auto (short 'p' <> long "port" <> value 3000)
    addMintetteOpts =
        AddMintette <$> strOption (long "host") <*> option auto (long "port") <*>
        strOption (long "key" <> help "Path to public key")

optionsParser :: Parser Options
optionsParser =
    Options <$> commandParser <*>
    strOption (long "path" <> value "db")

getOptions :: IO Options
getOptions =
    execParser $
    info (helper <*> optionsParser) (fullDesc <> progDesc "RSCoin's Bank")
