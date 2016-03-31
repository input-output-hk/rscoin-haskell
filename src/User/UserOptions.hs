-- | Command line options interface for user

module UserOptions
       ( UserOptions (..)
       , UserCommand (..)
       , DumpCommand (..)
       , getUserOptions
       ) where

import           RSCoin.Core            (Severity (Info), defaultAccountsNumber,
                                         defaultSecretKeyPath, PeriodId)

import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Options.Applicative    (Parser, auto, command, execParser,
                                         fullDesc, help, helper, info, long,
                                         option, progDesc, showDefault, some,
                                         subparser, switch, value)

import           Serokell.Util.OptParse (strOption)

-- | Input user command that's contained in every program call
data UserCommand
    = ListAddresses                 -- ^ List all addresses in wallet,
                                    -- starting with 1
    | UpdateBlockchain              -- ^ Query bank to update wallet
                                    -- state according to blockchain
                                    -- status
    | FormTransaction [(Int, Int64)]
                      (Text, Int64) -- ^ First argument represents
                                    -- inputs -- pairs (a,b), where a
                                    -- is index (starting from 1) of
                                    -- address in wallet, b is
                                    -- positive integer representing
                                    -- value to send.  Second argument
                                    -- represents the address to send,
                                    -- and amount
    | Dump DumpCommand
    deriving (Show)

data DumpCommand
    = DumpHBlocks PeriodId PeriodId
    | DumpHBlock PeriodId
    deriving (Show)

-- | Datatype describing user command line options
data UserOptions = UserOptions
    { userCommand  :: UserCommand -- ^ Command for the program to process
    , isBankMode   :: Bool        -- ^ If creating wallet in bank-mode,
    , bankModePath :: FilePath    -- ^ Path to bank's secret key
    , addressesNum :: Int         -- ^ Number of addresses to create initially
    , walletPath   :: FilePath    -- ^ Path to the wallet
    , logSeverity  :: Severity    -- ^ Logging severity
    } deriving (Show)

userCommandParser :: Parser UserCommand
userCommandParser =
    subparser
        (command
             "list-addresses"
             (info
                  (pure ListAddresses)
                  (progDesc
                       ("List all available addresses from wallet " <>
                        "and information about them."))) <>
         command
             "update-blockchain"
             (info
                  (pure UpdateBlockchain)
                  (progDesc "Query bank to sync local state with blockchain.")) <>
         command
             "form-transaction"
             (info formTransactionOpts (progDesc "Form and send transaction.")) <>
         command
             "dump"
             (info
                  (Dump <$> dumpCommandParser)
                  (progDesc "Dump Bank data")))
  where
    formTransactionOpts =
        FormTransaction <$>
        (some $
         option
             auto
             (long "addrfrom" <>
              help
                  ("Pairs (a,b) where 'a' is id of address as numbered in list-wallets " <>
                   "output, 'b' is integer -- amount of value to send."))) <*>
        ((,) <$> strOption
                 (long "addrto" <> help "Address to send coins to.") <*>
         option auto (long "value" <> help "Value to send."))

dumpCommandParser :: Parser DumpCommand
dumpCommandParser =
    subparser
        (command
             "blocks"
             (info
                  (DumpHBlocks
                      <$> option auto (long "from" <> help "Dump from which block")
                      <*> option auto (long "to" <> help "Dump to which block")
                  )
                  (progDesc ("Dump Bank high level blocks."))) <>
         command
             "block"
             (info
                  (DumpHBlock
                      <$> option auto (long "id" <> help "Dump block with specific periodId")
                  )
                  (progDesc ("Dump Bank high level block."))))

userOptionsParser :: FilePath -> Parser UserOptions
userOptionsParser dskp =
    UserOptions <$> userCommandParser <*>
    switch
        (long "bank-mode" <>
         help
             ("Start the client in bank-mode. " <>
              "Is needed only on wallet initialization. " <>
              "Will load bank's secret key.")) <*>
    strOption
        (long "bank-sk-path" <> help "Path to bank's secret key." <> value dskp <>
         showDefault) <*>
    option
        auto
        (long "addresses-num" <>
         help
             ("The number of addresses to create " <>
              "initially with the wallet") <>
         value defaultAccountsNumber <>
         showDefault) <*>
    strOption
        (long "wallet-path" <> help "Path to wallet database." <>
         value "wallet-db" <>
         showDefault) <*>
    option auto (long "log-severity" <> value Info <> showDefault)

-- | IO call that retrieves command line options
getUserOptions :: IO UserOptions
getUserOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> userOptionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin user client")
