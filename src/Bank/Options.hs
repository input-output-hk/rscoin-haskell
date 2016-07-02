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

import           RSCoin.Core            (Severity (Error), defaultPeriodDelta,
                                         defaultSecretKeyPath)

data Command
    = Serve FilePath
    | AddMintette String Int T.Text
    | AddAddress T.Text String

data Options = Options
    { cloCommand     :: Command
    , cloPath        :: FilePath
    , cloPeriodDelta :: Integer
    , cloLogSeverity :: Severity
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
             (info addMintetteOpts (progDesc "Add given mintette to database")) <>
         command
             "add-address"
             (info addAddressOpts (progDesc "Add given address and corresponding strategy to database"))
             )
  where
    serveOpts =
        Serve <$>
        strOption
            (short 'k' <> long "secret-key" <> help "Path to secret key" <>
             value defaultSKPath <>
             showDefault <>
             metavar "PATH TO KEY")
    addAddressOpts =
        AddAddress <$> strOption (long "address" <> help "Public key, determining address") <*>
        strOption
            (long "strategy" <>
             help "Address's strategy (directly, not from file). Example: 'MOfNStrategy 5 (fromList [\"YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM=\"])'" <>
             metavar "STRATEGY")
    addMintetteOpts =
        AddMintette <$> strOption (long "host") <*> option auto (long "port") <*>
        strOption
            (long "key" <>
             help "Mintette's public key (directly, not from file)" <>
             metavar "PUBLIC KEY")

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options <$> commandParser defaultSKPath <*>
    strOption
        (long "path" <> value "bank-db" <> showDefault <>
         help "Path to database") <*>
    option
        auto
        (long "period-delta" <> value (toInteger defaultPeriodDelta) <>
         showDefault <> help "Period length in seconds") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Bank")
