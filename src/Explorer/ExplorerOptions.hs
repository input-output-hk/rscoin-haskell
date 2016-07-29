-- | Command line options for Explorer

module ExplorerOptions
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         option, progDesc, showDefault, value,
                                         (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationFileName,
                                         defaultPort, defaultSecretKeyPath)

data Options = Options
    { cloPortRpc       :: Int
    , cloPortWeb       :: Int
    , cloPath          :: FilePath
    , cloSecretKeyPath :: FilePath
    , cloLogSeverity   :: Severity
    , cloConfigPath    :: FilePath
    }

optionsParser :: FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir =
    Options <$>
    option auto (mconcat [long "port-rpc", value defaultPort, showDefault]) <*>
    option
        auto
        (mconcat [long "port-web", value (defaultPort + 1), showDefault]) <*>
    strOption
        (mconcat
             [ long "path"
             , value (configDir </> "explorer-db")
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
             [ long "config-path"
             , help "Path to configuration file"
             , value defaultConfigurationFileName
             , showDefault])


getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir)
            (fullDesc <> progDesc "RSCoin Block Explorer")
