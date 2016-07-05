-- | Command line options interface for user

module UserOptions
       ( UserOptions (..)
       , UserCommand (..)
       , DumpCommand (..)
       , getUserOptions
       ) where

import           RSCoin.Core            (MintetteId, PeriodId, Severity (Error),
                                         defaultAccountsNumber, defaultBankHost,
                                         defaultSecretKeyPath)
import           RSCoin.User.Cache      (UserCache)

import           Data.ByteString        (ByteString)
import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Options.Applicative    (Parser, argument, auto, command,
                                         execParser, fullDesc, help, helper,
                                         info, long, many, metavar, option,
                                         progDesc, showDefault, some, subparser,
                                         switch, value)

import           Serokell.Util.OptParse (strOption)

-- | Command that describes single action from command-line interface
-- POV
data UserCommand
    = StartGUI                       -- ^ Start graphical user interface
    | ListAddresses                  -- ^ List all addresses in wallet,
                                     -- starting with 1
    | UpdateBlockchain               -- ^ Query bank to update wallet
                                     -- state according to blockchain
                                     -- status
    | FormTransaction [(Word, Int64, Int)]
                      Text
                      [(Int64, Int)]
                      (Maybe UserCache) -- ^ First argument represents
                                        -- inputs -- pairs (a,b,c), where a
                                        -- is index (starting from 1) of
                                        -- address in wallet, b is
                                        -- positive integer representing
                                        -- value to send. c is color.
                                        -- Second argument
                                        -- represents the address to send,
                                        -- and amount. Forth argument is optional cache
    | AddMultisigAddress Int [Text]
    | Dump DumpCommand
    deriving (Show)

data DumpCommand
    = DumpHBlocks PeriodId PeriodId
    | DumpHBlock PeriodId
    | DumpMintettes
    | DumpAddresses
    | DumpPeriod
    | DumpLogs MintetteId Int Int
    | DumpMintetteUtxo MintetteId
    | DumpMintetteBlocks MintetteId PeriodId
    | DumpMintetteLogs MintetteId PeriodId
    | DumpAddress Word
    deriving (Show)

-- | Datatype describing user command line options
data UserOptions = UserOptions
    { userCommand  :: UserCommand -- ^ Command for the program to process
    , isBankMode   :: Bool        -- ^ If creating wallet in bank-mode,
    , bankModePath :: FilePath    -- ^ Path to bank's secret key
    , addressesNum :: Int         -- ^ Number of addresses to create initially
    , walletPath   :: FilePath    -- ^ Path to the wallet
    , guidbPath    :: FilePath    -- ^ Path to the gui database.
    , logSeverity  :: Severity    -- ^ Logging severity
    , bankHost     :: ByteString
    } deriving (Show)

userCommandParser :: Parser UserCommand
userCommandParser =
    subparser
        (command
             "start-gui"
             (info (pure StartGUI) (progDesc "Start graphical user interface.")) <>
         command
             "list"
             (info
                  (pure ListAddresses)
                  (progDesc
                       ("List all available addresses from wallet " <>
                        "and information about them."))) <>
         command
             "update"
             (info
                  (pure UpdateBlockchain)
                  (progDesc "Query bank to sync local state with blockchain.")) <>
         command
             "send"
             (info formTransactionOpts (progDesc "Form and send transaction.")) <>
         command
             "addMultisig"
             (info addMultisigOpts (progDesc "Form and send transaction.")) <>
         command
             "dump-blocks"
             (info
                  (fmap Dump $
                   DumpHBlocks <$>
                   argument
                       auto
                       (metavar "FROM" <> help "Dump from which block") <*>
                   argument auto (metavar "TO" <> help "Dump to which block"))
                  (progDesc "Dump Bank high level blocks.")) <>
         command
             "dump-block"
             (info
                  (fmap Dump $
                   DumpHBlock <$>
                   argument
                       auto
                       (metavar "ID" <>
                        help "Dump block with specific periodId"))
                  (progDesc "Dump Bank high level block.")) <>
         command
             "dump-addresses"
             (info
                  (pure $ Dump DumpAddresses)
                  (progDesc "Dump list of addresses.")) <>
         command
             "dump-mintettes"
             (info
                  (pure $ Dump DumpMintettes)
                  (progDesc "Dump list of mintettes.")) <>
         command
             "dump-period"
             (info (pure $ Dump DumpPeriod) (progDesc "Dump last period.")) <>
         command
             "dump-logs"
             (info
                  (fmap Dump $
                   DumpLogs <$>
                   argument
                       auto
                       (metavar "MINTETTE_ID" <>
                        help "Dump logs of mintette with this id.") <*>
                   argument
                       auto
                       (metavar "FROM" <> help "Dump from which entry.") <*>
                   argument auto (metavar "TO" <> help "Dump to which entry."))
                  (progDesc
                       "Dump action logs of corresponding mintette, range or entries.")) <>
         command
             "dump-mintette-utxo"
             (info
                  (fmap Dump $
                   DumpMintetteUtxo <$>
                   argument
                       auto
                       (metavar "MINTETTE_ID" <>
                        help "Dump utxo of mintette with this id."))
                  (progDesc "Dump utxo of corresponding mintette.")) <>
         command
             "dump-mintette-blocks"
             (info
                  (fmap Dump . DumpMintetteBlocks <$>
                   argument
                       auto
                       (metavar "MINTETTE_ID" <>
                        help "Dump blocks of mintette with this id.") <*>
                   argument
                       auto
                       (metavar "PERIOD_ID" <>
                        help "Dump blocks with this period id."))
                  (progDesc
                       "Dump blocks of corresponding mintette and periodId.")) <>
         command
             "dump-mintette-logs"
             (info
                  (fmap Dump . DumpMintetteLogs <$>
                   argument
                       auto
                       (metavar "MINTETTE_ID" <>
                        help "Dump logs of mintette with this id.") <*>
                   argument
                       auto
                       (metavar "PERIOD_ID" <>
                        help "Dump logs with this period id."))
                  (progDesc "Dump logs of corresponding mintette and periodId.")) <>
         command
             "dump-address"
             (info
                  (fmap Dump $
                   DumpAddress <$>
                   argument
                       auto
                       (metavar "INDEX" <> help "Index of address to dump"))
                  (progDesc "Dump address with given index.")))
  where
    formTransactionOpts =
        FormTransaction <$>
        some
            (option
                 auto
                 (long "from" <>
                  help
                      ("Tuples (a,b,c) where " <>
                       "'a' is id of address as numbered in list-wallets output, " <>
                       "'b' is integer -- amount of coins to send, " <>
                       "'c' is the color (0 for uncolored), any uncolored ~ colored."))) <*>
        strOption (long "toaddr" <> help "Address to send coins to.") <*>
        many
            (option
                 auto
                 (long "tocoin" <>
                  help
                      ("Pairs (a,b) where " <>
                       "'a' is amount of coins to send, " <>
                       "'b' is the color of that coin")))
        -- FIXME: should we do caching here or not?
        <*>
        pure Nothing
    addMultisigOpts =
        AddMultisigAddress <$>
        option auto (long "m" <> help "Number m from m/n") <*>
        many (strOption (long "addr" <> help "Addresses that would own"))

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
    strOption
        (long "guidb-path" <> help "Path to gui database" <>
         value "gui-db" <>
         showDefault) <*>
    option auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    strOption
        (long "bank-host" <> value defaultBankHost <> showDefault <>
         help "Host name for bank")

-- | IO call that retrieves command line options
getUserOptions :: IO UserOptions
getUserOptions = do
    defaultSKPath <- defaultSecretKeyPath
    execParser $
        info
            (helper <*> userOptionsParser defaultSKPath)
            (fullDesc <> progDesc "RSCoin user client")
