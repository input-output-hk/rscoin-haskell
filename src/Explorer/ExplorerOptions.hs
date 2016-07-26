-- | Command line options for Explorer

module ExplorerOptions
       ( Options (..)
       , getOptions
       ) where

import           Data.ByteString        (ByteString)
import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         option, progDesc, showDefault, value,
                                         (<>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), defaultPort,
                                         defaultSecretKeyPath, localhost)

data Options = Options
    { cloPortRpc       :: Int
    , cloPortWeb       :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    , cloLogSeverity   :: Severity
    , cloBankHost      :: ByteString
    }

optionsParser :: FilePath -> Parser Options
optionsParser defaultSKPath =
    Options <$>
    option
        auto
        (mconcat [long "port-rpc", value defaultPort, showDefault]) <*>
    option
        auto
        (mconcat [long "port-web", value (defaultPort + 1), showDefault]) <*>
    strOption
        (mconcat
             [ long "path"
             , value "explorer-db"
             , showDefault
             , help "Path to database"]) <*>
    strOption
        (mconcat
             [long "sk", value defaultSKPath, metavar "FILEPATH", showDefault]) <*>
    option
        auto
        (mconcat
             [ long "log-severity"
             , value Error
             , showDefault
             , help "Logging severity"]) <*>
    strOption
        (mconcat
             [ long "bank-host"
             , value localhost
             , showDefault
             , help "Host name for bank"])

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin Block Explorer")
