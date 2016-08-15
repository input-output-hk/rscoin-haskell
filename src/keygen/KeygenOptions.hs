-- | Command line options for keygen

module KeygenOptions
       ( KeyGenCommand (..)
       , Options (..)
       , getOptions
       ) where

import           Options.Applicative      (Parser, auto, command, execParser,
                                           fullDesc, info, help, helper, long,
                                           metavar, option, optional, progDesc,
                                           short, subparser, (<>))

import           Serokell.Util.OptParse   (strOption)

data KeyGenCommand = Single FilePath
                   | Batch Int FilePath FilePath
                   | Derive FilePath

data Options = Options
    { cloCommand       :: KeyGenCommand
    , cloPublicKeyPath :: Maybe FilePath
    }

commandParser :: Parser KeyGenCommand
commandParser =
    subparser
        (command
             "single"
              (info
                   singleOpts
                   (progDesc singleDesc)) <>
        command
            "batch"
            (info
                 batchOpts
                 (progDesc batchDesc)) <>
        command
            "derive"
            (info
                 deriveOpts
                 (progDesc deriveDesc)))
  where
    singleOpts =
        Single <$>
        generatedKeys
    batchOpts =
        Batch <$>
        option
            auto
            (short 'n' <> long "key-number" <> help numKeyHelpStr <>
             metavar "NUMBER OF KEYS") <*>
        generatedKeys <*>
        secretKey
    deriveOpts =
        Derive <$>
        secretKey

    generatedKeys =
        strOption
            (short 'k' <> long "keys-path" <> help genKeyHelpStr <>
             metavar "PATH TO KEYS")

    secretKey =
        strOption
            (short 's' <> long "secret-key-path" <> help secKeyHelpStr <>
             metavar "PATH TO SECRET KEY")

    numKeyHelpStr = "Number of keys generated"
    genKeyHelpStr = "Path to generated keys and signatures"
    secKeyHelpStr = "Path to master secret key"
    singleDesc    = "Generate a single pair of public and secret keys"
    batchDesc     = "Generate array of public keys, secret keys and signatures"
    deriveDesc    = "Derive public key from the given secret key"

optionsParser :: Parser Options
optionsParser =
    Options <$> commandParser <*> publicKeyPath
  where
    publicKeyPath =
        optional
        (strOption
            (short 'p' <> long "public-key-path" <> help pubKeyHelpStr <>
             metavar "PATH TO PUBLIC KEY"))
    pubKeyHelpStr = "Path to the Public key"


getOptions :: IO Options
getOptions = do
    execParser $
        info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "RSCoin's keygen")
