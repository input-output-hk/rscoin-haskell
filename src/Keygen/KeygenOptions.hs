-- | Command line options for keygen

module KeygenOptions
       ( KeyGenCommand (..)
       , getOptions
       ) where

import           Options.Applicative    (Parser, auto, command, execParser,
                                         fullDesc, help, helper, info, long,
                                         metavar, option, optional, progDesc,
                                         short, subparser, (<>))

import           Serokell.Util.OptParse (strOption)

type Output = Maybe FilePath
data KeyGenCommand = Single FilePath
                   | Batch Int FilePath FilePath
                   | Derive FilePath Output
                   | Sign FilePath FilePath Output

optionsParser :: Parser KeyGenCommand
optionsParser =
    subparser
        (command "single" (info singleOpts (progDesc singleDesc)) <>
         command "batch" (info batchOpts (progDesc batchDesc)) <>
         command "derive" (info deriveOpts (progDesc deriveDesc)) <>
         command "sign" (info signOpts (progDesc signDesc)))
  where
    singleOpts = Single <$> generatedKeys
    batchOpts =
        Batch <$>
        option
            auto
            (short 'n' <> long "key-number" <> help numKeyHelpStr <>
             metavar "INTEGER") <*>
        generatedKeys <*>
        secretKey
    deriveOpts = Derive <$> secretKey <*> output
    signOpts = Sign <$> secretKey <*> publicKey <*> output
    generatedKeys =
        strOption
            (short 'k' <> long "keys-path" <> help genKeyHelpStr <>
             metavar "FILEPATH")
    publicKey =
        strOption
            (short 'p' <> long "public-key-path" <> help pubKeyHelpStr <>
             metavar "FILEPATH")
    secretKey =
        strOption
            (short 's' <> long "secret-key-path" <> help secKeyHelpStr <>
             metavar "FILEPATH")
    output = optional $ strOption
         (short 'o' <> long "output" <> help outputDesc <>
          metavar "FILEPATH")
    numKeyHelpStr = "Number of keys generated"
    genKeyHelpStr = "Path to generated keys and signatures"
    secKeyHelpStr = "Path to master secret key"
    pubKeyHelpStr = "Path to master public key"
    singleDesc = "Generate a single pair of public and secret keys"
    batchDesc = "Generate array of public keys, secret keys and signatures"
    deriveDesc = "Derive public key from the given secret key"
    signDesc = "Sign public key of A with secret key of B"
    outputDesc = "Filepath to write output to. Default is usually " <>
                 "some of inputs with .smth suffix"

getOptions :: IO KeyGenCommand
getOptions =
    execParser $
    info (helper <*> optionsParser) (fullDesc <> progDesc "RSCoin's keygen")
