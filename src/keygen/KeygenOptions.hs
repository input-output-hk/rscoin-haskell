-- | Command line options for keygen

module KeygenOptions
       ( ) where

import qualified Data.Text                as T
import           Options.Applicative      (Parser, auto, command, execParser,
                                           fullDesc, info, help, helper,
                                           long, option, progDesc, showDefault,
                                           subparser, value, (<>))

import           Serokell.Util.OptParse   (strOption)

import RSCoin.Core                        (Severity (Error))

data Command = GenerateSingle | GenerateBatch Integer

data Options = Options
    { cloCommand     :: Command
    , cloPubKeyNum   :: Integer
    , cloKeysPath    :: FilePath
    , cloLogSeverity :: Severity
    , cloSKPath      :: FilePath
    }

defaultPubKeyNum = 100

commandParser =
    subparser
        (command
             "generate-single"
              (info
                   generateSOpts
                   (progDesc "Generate array of public keys, secret kets and signatures")) <>
        command
             "generate-batch"
              (info
                   generateBOpts
                   (progDesc "Generate array of public keys, secret kets and signatures")))
  where
    generateSOpts = pure GenerateSingle
    generateBOpts =
        GenerateBatch <*> option auto (long "keynum") <*>
        option
            auto
            (long "key-number" <> help "Number ofkeys generated" <>
             value (toInteger defaultPubKeyNum) <>
             showDefault)

optionsParser :: FilePath -> FilePath -> Parser Options
optionsParser defaultSKPath defaultStrgPath =
    Options <$> commandParser <*>
    strOption
        (long "keys-path" <> value defaultStrgPath <> showDefault <>
        help "Path to generated keys") <*>
    option
        auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    strOption
        (long "sk-path" <> help "Path to secret key" <>
         value defaultSKPath <>
         showDefault)

getOptions :: IO Options
getOptions = do
    defaultSKPath <- defaultSecretKeyPath
    defaultStrgPath <- defaultStoragePath
    execParser $
        info
            (helper <*> optionsParser defaultSKPath defaultStrgPath)
            (fullDesc <> progDesc "RSCoin's keygen")
