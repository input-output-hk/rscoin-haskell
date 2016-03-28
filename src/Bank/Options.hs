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

import           RSCoin.Core            (defaultPort, defaultSecretKeyPath)

data Command
    = Serve FilePath
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
        Serve <$>
        strOption
            (short 'k' <> long "secret-key" <> help "Path to secret key" <>
             value defaultSecretKeyPath <>
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
