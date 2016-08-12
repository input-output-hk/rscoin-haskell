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

data KeyGenCommand = Single FilePath | Batch Int FilePath FilePath

data Options = Options
    { cloCommand     :: KeyGenCommand
    }

commandParser :: Parser KeyGenCommand
commandParser =
    subparser
        (command
             "single"
              (info
                   generateSOpts
                   (progDesc singleDesc)) <>
        command
            "batch"
            (info generateBOpts (progDesc batchDesc)))
  where
    generateSOpts =
        Single <$>
        generatedKeys
    generateBOpts =
        Batch <$>
        option
            auto
            (short 'n' <> long "key-number" <> help numKeyHelpStr <>
             metavar "NUMBER OF KEYS") <*>
        generatedKeys <*>
        masterSecretKey

    generatedKeys =
        strOption
            (short 'k' <> long "keys-path" <> help genKeyHelpStr <>
             metavar "PATH TO KEYS")

    masterSecretKey =
        strOption
            (short 's' <> long "secret-key-path" <> help secKeyHelpStr <>
             metavar "PATH TO SECRET KEY")

    numKeyHelpStr = "Number of keys generated"
    genKeyHelpStr = "Path to generated keys and signatures"
    secKeyHelpStr = "Path to master secret key"
    singleDesc    = "Generate a single pair of public and secret keys"
    batchDesc     = "Generate array of public keys, secret keys and signatures"

optionsParser :: Parser Options
optionsParser =
    Options <$> commandParser

getOptions :: IO Options
getOptions = do
    execParser $
        info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "RSCoin's keygen")
