-- | Command line options interface for user

module UserOptions
       ( UserOptions (..)
       , UserCommand (..)
       , getUserOptions
       ) where

import           Data.Monoid         ((<>))
import           Options.Applicative (Parser, auto, command, execParser,
                                      fullDesc, help, helper, info, long,
                                      option, progDesc, some, strOption,
                                      subparser, value)

data UserCommand
    = ListWallets
    | UpdateBlockchain
    | FormTransaction [Int]
                      [(String, Int)]
    deriving (Show)

data UserOptions = UserOptions
    { userCommand :: UserCommand
    , walletPath  :: FilePath
    } deriving (Show)

userCommandParser :: Parser UserCommand
userCommandParser =
    subparser
        (command
             "list-wallets"
             (info
                  (pure ListWallets)
                  (progDesc
                       "List all available wallets and information about them.")) <>
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
             (long "addrid" <>
              help "Id of address as numbered in list-wallets output.")) <*>
        (some $
         (,) <$>
         option auto (long "addrout" <> help "Address to send coins to.") <*>
         option auto (long "value" <> help "Value to send."))

userOptionsParser :: Parser UserOptions
userOptionsParser =
    UserOptions <$> userCommandParser <*>
    strOption
        (long "wallet-path" <> help "Path to wallet database." <>
         value "wallet-db")

getUserOptions :: IO UserOptions
getUserOptions =
    execParser $
    info (helper <*> userOptionsParser) (fullDesc <> progDesc "RSCoin user client")
