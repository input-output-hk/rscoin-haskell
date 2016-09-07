-- | Command line options for tests

module TestOptions
       ( FullTestConfig (..)
       , TestVar
       , testTVar
       , getOptions
       ) where

import           Control.Concurrent.STM.TVar (TVar, newTVarIO)
import           Data.Default                (Default (def))
import           Options.Applicative         (Parser, (<>), auto, execParser, fullDesc,
                                              help, helper, info, long, metavar, option,
                                              progDesc, short, switch)
import           RSCoin.Core                 (Severity (..))
import           System.IO.Unsafe            (unsafePerformIO)

data FullTestConfig = FullTestConfig
    { ftcGlobalSeverity   :: !Severity
    , ftcBankSeverity     :: !(Maybe Severity)
    , ftcMintetteSeverity :: !(Maybe Severity)
    , ftcUserSeverity     :: !(Maybe Severity)
    , ftcTestingSeverity  :: !(Maybe Severity)
    , ftcRealMode         :: !Bool
    } deriving (Show)

instance Default FullTestConfig where
    def =
        FullTestConfig
        { ftcGlobalSeverity = Warning
        , ftcBankSeverity = def
        , ftcMintetteSeverity = def
        , ftcUserSeverity = def
        , ftcTestingSeverity = Just Warning
        , ftcRealMode = False
        }

type TestVar = TVar FullTestConfig

testTVar :: TestVar
testTVar = unsafePerformIO (newTVarIO def)

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
