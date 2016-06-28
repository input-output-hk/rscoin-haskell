-- | Command line options for Signer

module Options
       ( Options (..)
       , getOptions
       ) where

import           Data.ByteString        (ByteString)
import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         option, progDesc, short, showDefault,
                                         switch, value, (<>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), defaultBankHost,
                                         defaultPort, defaultSecretKeyPath)

data Options = Options
    { cliPort          :: Int
    , cliPath          :: FilePath
    , cliSecretKeyPath :: FilePath
    , cliLogSeverity   :: Severity
    , cliMemMode       :: Bool
    , cliBankHost      :: ByteString
    , cliMode          :: Mode
    }

data Mode = Immediate
            | Delayed { cliSignDelay   :: Int
                      , cliSignApprove :: Bool
                      }

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options <$>
    option auto (short 'p' <> long "port" <> value defaultPort <> showDefault) <*>
    strOption
        (long "path" <> value "signer-db" <> showDefault <>
         help "Path to database") <*>
    strOption
        (long "sk" <> value defaultSKPath <> metavar "FILEPATH" <> showDefault) <*>
    option auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    switch (short 'm' <> long "memory-mode" <> help "Run in memory mode") <*>
    strOption
        (long "bank-host" <> value defaultBankHost <> showDefault <>
         help "Host name for bank") <*> modeParser

modeParser :: Parser Mode
modeParser = subparser $
  command "immediate" (info (pure Immediate) (progDesc "Immediately sign all incomming transactions.")) <>
  command "delayed" (info (Delayed <$> delayParser <*> respTypeParser) (progDesc "Immediately sign all incomming transactions."))
    where delayParser = option auto (long "delay" <> value defaultSignDelay <> showDefault)
          respTypeParser = not <$> switch (long "reject" <> help "Reject transactions")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Signer")
