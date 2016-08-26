-- | Command line options for Explorer

module ExplorerOptions
       ( Options (..)
       , getOptions
       ) where

import           Options.Applicative    (Parser, auto, execParser, fullDesc,
                                         help, helper, info, long, metavar,
                                         option, progDesc, short, showDefault,
                                         switch, value, (<>))
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (Severity (Error), configDirectory,
                                         defaultConfigurationPath, defaultPort,
                                         defaultSecretKeyPath)

data Options = Options
    { cloPortRpc        :: Int
    , cloPortWeb        :: Int
    , cloPath           :: FilePath
    , cloSecretKeyPath  :: FilePath
    , cloAutoCreateKey  :: Bool
    , cloLogSeverity    :: Severity
    , cloConfigPath     :: FilePath
    , cloDefaultContext :: Bool
    , cloRebuildDB      :: Bool
    }

optionsParser :: FilePath -> FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath configDir defaultConfigPath =
    Options <$>
    option
        auto
        (mconcat
             [ long "port-rpc"
             , value defaultPort
             , help "Port to communicate with bank on"
             , showDefault
             , metavar "PORT"]) <*>
    option
        auto
        (mconcat
             [ long "port-web"
             , value (defaultPort + 1)
             , help "Port to communicate with web requests on"
             , showDefault
             , metavar "PORT"]) <*>
    strOption
        (mconcat
             [ long "path"
             , value (configDir </> "explorer-db")
             , showDefault
             , help "Path to database"
             , metavar "FILEPATH"]) <*>
    strOption
        (mconcat
             [ long "sk"
             , value defaultSKPath
             , metavar "FILEPATH"
             , help "Path to the secret key"
             , showDefault]) <*>
    switch
        (long "auto-create-sk" <>
         help
             ("If the \"sk\" is pointing to non-existing " <>
              "file, generate a keypair")) <*>
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
             , value defaultConfigPath
             , showDefault]) <*>
    switch
        (mconcat
             [ short 'd'
             , long "default-context"
             , help
                   ("Use default NodeContext. " <>
                    "Intended to be used for local deployment")]) <*>
    switch
        (mconcat
             [ short 'r'
             , long "rebuild-db"
             , help
                   ("Erase database if it already exists")])


getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin Block Explorer")
