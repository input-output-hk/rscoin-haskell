-- | Command line options for keygen

module KeygenOptions
       ( KeyGenCommand (..)
       , Options (..)
       , getOptions
       ) where

import           Options.Applicative      (Parser, auto, command, execParser,
                                           fullDesc, info, help, helper, long,
                                           metavar, option, progDesc, short,
                                           showDefault, subparser, value, (<>))

import           Serokell.Util.OptParse   (strOption)

import RSCoin.Core                        (Severity (Error))

data KeyGenCommand = Single FilePath FilePath | Batch Int FilePath FilePath

data Options = Options
    { cloCommand     :: KeyGenCommand
    , cloLogSeverity :: Severity
    }

commandParser :: Parser KeyGenCommand
commandParser =
    subparser
        (command
             "generate-single"
              (info
                   generateSOpts
                   (progDesc "Generate array of public keys, secret keys and signatures")) <>
        command
            "generate-batch"
            (info generateBOpts (progDesc "Generate array of keys and signatures")))
  where
    generateSOpts =
        Single <$>
        generatedKeys <*>
        masterSecretKey
    generateBOpts =
        Batch <$>
        option
            auto
            (long "key-number" <> help "Number ofkeys generated") <*>
        generatedKeys <*>
        masterSecretKey
    generatedKeys =
        strOption
            (short 'g' <> long "keys-path" <> help gkHelpStr <>
             metavar "PATH TO KEYS")
    masterSecretKey =
        strOption
            (short 'k' <> long "secret-key-path" <> help skHelpStr <>
             metavar "PATH TO SECRET KEY")
    gkHelpStr = "Path to generated keys and signatures"
    skHelpStr = "Path to master secret key"

optionsParser :: Parser Options
optionsParser =
    Options <$> commandParser <*>
    option
        auto
        (short 'l' <> long "log-severity" <> help "Logging severity" <>
         value Error <> showDefault <>
         metavar "LOG-SEVERITY")

getOptions :: IO Options
getOptions = do
    execParser $
        info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "RSCoin's keygen")
