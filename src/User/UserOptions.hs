-- | Command line options interface for user

module UserOptions
       ( UserOptions (..)
       , UserCommand (..)
       , DumpCommand (..)
       , getUserOptions
       ) where

import           Control.Applicative    (optional)
import           Data.Int               (Int64)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Options.Applicative    (Parser, argument, auto, command,
                                         execParser, fullDesc, help, helper,
                                         info, long, many, metavar, option,
                                         progDesc, short, showDefault, some,
                                         subparser, switch, value)
import           System.FilePath        ((</>))

import           Serokell.Util.OptParse (strOption)

import           RSCoin.Core            (MintetteId, PeriodId, Severity (Error),
                                         configDirectory, defaultAccountsNumber,
                                         defaultConfigurationPath,
                                         defaultSecretKeyPath)
import           RSCoin.User            (UserCache)


-- | Command that describes single action from command-line interface
-- POV
data UserCommand
    -- | Start graphical user interface
    = StartGUI
    -- | List all addresses in wallet, starting with 1
    | ListAddresses
    -- | Query bank to update wallet state according to blockchain
    -- status
    | UpdateBlockchain
    -- | First argument represents inputs -- pairs (a,b,c), where a is
    -- index (starting from 1) of address in wallet, b is positive
    -- integer representing value to send. c is color.  Second
    -- argument represents the address to send, and amount. Forth
    -- argument is optional cache
    | FormTransaction [(Word, Int64, Int)]
                      Text
                      [(Int64, Int)]
                      (Maybe UserCache)
    -- | First argument represents number m of required signatures from addr;
    -- second -- list of user parties' in addresses;
    -- third -- list of trust parties' in addresses;
    -- fourth is Nothing if we need to generate multisignature address.
    | AddMultisigAddress Int
                         [Text]
                         [Text]
                         (Maybe Text)
    -- | List all addresses in which current user acts like party
    | ListAllocations
    -- | For a request #N in local list send confirmation to a Notary
    | ConfirmAllocation Int
    -- | Add a local address to storage (filepaths to sk and pk, then
    -- blockchain heights to query -- minimum and maximum)
    | ImportAddress FilePath FilePath Int (Maybe Int)
    | Dump DumpCommand
    -- @TODO move to rscoin-keygen
    | SignSeed Text (Maybe FilePath)
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
    , configPath   :: FilePath    -- ^ Configuration file path
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
              "list-alloc"
              (info
                  (pure ListAllocations)
                  (progDesc "List all multisignature address allocations you need to confirm")
              ) <>
         command
             "update"
             (info
                  (pure UpdateBlockchain)
                  (progDesc "Query bank to sync local state with blockchain.")) <>
         command
             "send"
             (info formTransactionOpts (progDesc "Form and send transaction.")) <>
         command
             "add-multisig"
             (info addMultisigOpts (progDesc "Create multisignature address allocation")) <>
         command
              "confirm"
              (info
                  confirmOpts
                  (progDesc "Confirm MS address allocation from `rscoin-user list-alloc`")
              ) <>
         command
              "import-address"
              (info
                  importAddressOpts
                  (progDesc "Import address to storage given a (secretKey,publicKey) pair")
              ) <>
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
                  (progDesc "Dump address with given index.")) <>
        command
            "sign-seed"
             (info signSeedOpts (progDesc "Sign seed with key.")))
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
        AddMultisigAddress
        <$>
        option auto
            (short 'm' <> help "Number m from m/n")
        <*>
        many (strOption $
            long "uaddr" <> help "User party Addresses that would own this MS address")
        <*>
        many (strOption $
            long "taddr" <> help "Trust party Addresses that would own this MS address")
        <*>
        optional (strOption $
            long "ms-addr" <> help "New multisignature address")
    confirmOpts =
        ConfirmAllocation
        <$>
        option auto
            (short 'n' <> help "Index starting from 1 in `list-alloc`")
    importAddressOpts =
        ImportAddress
        <$>
        (strOption $ long "skPath" <> help "Path to file with binary-encoded secret key")
        <*>
        (strOption $ long "pkPath" <> help "Path to file with base64-encoded public key")
        <*>
        (option auto $ long "queryFrom" <> help "Height to query blockchain from" <> value 0)
        <*>
        (option (Just <$> auto) (long "queryTo" <>
                                 help "Height to query blockchain to, default maxheight" <>
                                 value Nothing))
    signSeedOpts =
        SignSeed
        <$>
        (strOption $
            long "seed" <> help "Seed to sign")
        <*>
        optional (strOption $
            short 'k' <> long "secret-key" <> help "Path to secret key" <>
             metavar "PATH TO KEY")

userOptionsParser :: FilePath -> FilePath -> FilePath -> Parser UserOptions
userOptionsParser dskp configDir defaultConfigPath =
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
         value (configDir </> "wallet-db") <>
         showDefault) <*>
    strOption
        (long "guidb-path" <> help "Path to gui database" <>
         value "gui-db" <>
         showDefault) <*>
    option auto
        (long "log-severity" <> value Error <> showDefault <>
         help "Logging severity") <*>
    strOption
        (long "config-path" <> help "Path to configuration file" <>
         value defaultConfigPath <>
         showDefault)


-- | IO call that retrieves command line options
getUserOptions :: IO UserOptions
getUserOptions = do
    defaultSKPath <- defaultSecretKeyPath
    configDir <- configDirectory
    defaultConfigPath <- defaultConfigurationPath
    execParser $
        info
            (helper <*>
             userOptionsParser defaultSKPath configDir defaultConfigPath)
            (fullDesc <> progDesc "RSCoin user client")
