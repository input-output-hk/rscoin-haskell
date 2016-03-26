-- | Command line options interface for user

module UserOptions
       ( UserOptions (..)
       , UserCommand (..)
       , getUserOptions
       ) where

import           Data.Int            (Int64)
import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, auto, command, execParser,
                                      fullDesc, help, helper, info, long,
                                      option, progDesc, some, strOption,
                                      subparser, value)

-- | Input user command that's contained in every program call
data UserCommand
    = ListAddresses                   -- ^ List all addresses in wallet,
                                      -- starting with 1
    | UpdateBlockchain                -- ^ Query bank to update wallet
                                      -- state according to blockchain
                                      -- status
    | FormTransaction [(Int, Int64)]
                      (String, Int64) -- ^ First argument represents
                                      -- inputs -- pairs (a,b), where a
                                      -- is index (starting from 1) of
                                      -- address in wallet, b is
                                      -- positive integer representing
                                      -- value to send.  Second argument
                                      -- represents the address to send,
                                      -- and amount
    deriving (Show)

-- | Datatype describing user command line options
data UserOptions = UserOptions
    { userCommand :: UserCommand
    , walletPath  :: FilePath
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
             (info formTransactionOpts (progDesc "Form and send transaction.")))
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
        ((,) <$> strOption (long "addrout" <> help "Address to send coins to.") <*>
         option auto (long "value" <> help "Value to send."))

userOptionsParser :: Parser UserOptions
userOptionsParser =
    UserOptions <$> userCommandParser <*>
    strOption
        (long "wallet-path" <> help "Path to wallet database." <>
         value "wallet-db")

-- | IO call that retrieves command line options
getUserOptions :: IO UserOptions
getUserOptions =
    execParser $
    info (helper <*> userOptionsParser) (fullDesc <> progDesc "RSCoin user client")
