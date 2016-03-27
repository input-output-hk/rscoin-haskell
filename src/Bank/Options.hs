-- | Command line options for Bank

module Options
       ( Command (..)
       , Options (..)
       , getOptions
       ) where

import qualified Data.Text              as T
import           Options.Applicative    (Parser, auto, command, execParser,
                                         fullDesc, help, helper, info, long,
                                         metavar, option, progDesc, short,
                                         subparser, value, (<>))

import           Serokell.Util.OptParse (strOption)

data Command
    = Serve Int FilePath
    | AddMintette String Int T.Text

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
    serveOpts =
        Serve <$> option auto (short 'p' <> long "port" <> value 3000) <*>
        strOption
            (short 'k' <> long "secret-key" <> help "Path to secret key" <>
             metavar "PATH TO KEY")
    addMintetteOpts =
        AddMintette <$> strOption (long "host") <*> option auto (long "port") <*>
        strOption
            (long "key" <> help "Mintette's public key" <> metavar "PUBLIC KEY")

optionsParser :: Parser Options
optionsParser =
    Options <$> commandParser <*>
    strOption (long "path" <> value "db")

getOptions :: IO Options
getOptions =
    execParser $
    info (helper <*> optionsParser) (fullDesc <> progDesc "RSCoin's Bank")
