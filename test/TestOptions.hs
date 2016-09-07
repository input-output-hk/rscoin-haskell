-- | Command line options for tests

module TestOptions
       ( --TestOptions (..),
         TestVar
       , getOptions
       ) where

import           Control.Concurrent.STM.TVar (TVar)
import           Options.Applicative         (Parser, (<>), auto, execParser, fullDesc,
                                              help, helper, info, long, metavar, option,
                                              progDesc, short, switch)
import           Test.RSCoin.Full.FullSpec   (FullTestConfig (..))

{-data TestOptions = TestOptions
    { cloGlobalSeverity :: Severity
    , cloBankSeverity   :: Maybe Severity
    , cloMintetteSeverity :: Maybe Severity
    , cloUserSeverity     :: Maybe Severity
    , cloTestingSeverity  :: Maybe Severity
    , cloRealMode         :: Bool
    } deriving Show-}

optionsParser :: Parser FullTestConfig
optionsParser =
    FullTestConfig <$>
    option
        auto
        (short 'g' <>long "global-severity" <>
         help "Global logging severity" <>
         metavar "SEVERITY") <*>
    option
        auto
        (short 'b' <> long "bank-severity" <>
         help "Bank's logging severity" <>
         metavar "SEVERITY") <*>
    option
        auto
        (short 'm' <> long "mintette-severity" <>
         help "Mintette's logging severity" <>
         metavar "SEVERITY") <*>
    option
        auto
        (short 'u' <> long "user-severity" <>
         help "User's logging severity" <>
         metavar "SEVERITY") <*>
    option
        auto
        (short 't' <> long "testing-severity" <>
         help "Testing logging severity" <>
         metavar "SEVERITY") <*>
    switch
        (short 'r' <> long "real-mode" <>
         help "Run tests in real mode")

getOptions :: IO FullTestConfig
getOptions = do
    execParser $
        info
            (helper <*> optionsParser)
            (fullDesc <> progDesc "RSCoin's testing framework")

type TestVar = TVar FullTestConfig
