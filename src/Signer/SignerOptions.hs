-- | Command line options for Signer

module SignerOptions
        ( Options (..)
        , getOptions
        ) where

import           Data.ByteString        (ByteString)
import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long,
                                         option, progDesc, short, showDefault,
                                         switch, value, (<>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), defaultBankHost,
                                         defaultPort, defaultSecretKeyPath)

data Options = Options
    { cliPort          :: Int
    , cliPath          :: FilePath
    --, cliSecretKeyPath :: FilePath
    , cliLogSeverity   :: Severity
    , cliMemMode       :: Bool
    , cliBankHost      :: ByteString
    } deriving Show

--data Mode
--    = Immediate
--    | Delayed { cliSignDelay   :: Int
--              , cliSignApprove :: Bool
--              }
--    deriving Show

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options
    <$>
    --modeParser
    -- <*>
    option auto
        (short 'p' <> long "port" <> value defaultPort <> showDefault <>
         help "Signer port to receive transactions")
    <*>
    strOption
        (long "path" <> value "signer-db" <> showDefault <>
         help "Path to signer database")
    <*>
    --strOption
    --    (long "sk" <> value defaultSKPath <> metavar "FILEPATH" <> showDefault)
    -- <*>
    option auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity")
    <*>
    switch
        (short 'm' <> long "memory-mode" <> help "Run in memory mode")
    <*>
    strOption
        (long "bank-host" <> value defaultBankHost <> showDefault <>
         help "Host name for bank")

--modeParser :: Parser Mode
--modeParser = subparser $
--  command "immediate" (info (pure Immediate) (progDesc "Immediately sign all incomming transactions.")) <>
--  command "delayed" (info (Delayed <$> delayParser <*> respTypeParser) (progDesc "Immediately sign all incomming transactions."))
--    where delayParser = option auto (long "delay" <> value defaultSignDelay <> showDefault)
--          respTypeParser = not <$> switch (long "reject" <> help "Reject transactions")

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin's Signer")
