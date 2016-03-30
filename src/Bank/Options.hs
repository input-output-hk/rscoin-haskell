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
                                         showDefault, subparser, value, (<>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (defaultSecretKeyPath)

data Command
    = Serve FilePath
    | AddMintette String Int T.Text

data Options = Options
    { cloCommand :: Command
    , cloPath    :: FilePath
    }

commandParser :: FilePath -> Parser Command
commandParser defaultSKPath =
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
             value defaultSKPath <> showDefault <>
             metavar "PATH TO KEY")
    addMintetteOpts =
        AddMintette <$> strOption (long "host") <*> option auto (long "port") <*>
        strOption
            (long "key" <> help "Mintette's public key" <> metavar "PUBLIC KEY")

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options <$> commandParser defaultSKPath <*>
    strOption
        (long "path" <> value "bank-db" <> showDefault <> help "Path to database")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Bank")
