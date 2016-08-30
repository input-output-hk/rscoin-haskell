{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | Types used within Explorer WebSockets Server for communication
-- with client.

module RSCoin.Explorer.Web.Sockets.Types
       ( ServerError (..)
       , ErrorableMsg
       , IntroductoryMsg (..)
       , AddressInfoMsg (..)
       , OutcomingMsg (..)
       , CoinsMapSummary
       , TransactionSummary
       ) where

import           Data.Aeson                (FromJSON, eitherDecode, encode)
import           Data.Aeson.TH             (deriveJSON, deriveToJSON)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Either.Combinators   (mapLeft)
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import qualified Network.WebSockets        as WS

import           Serokell.Aeson.Options    (defaultOptionsPS)

import qualified RSCoin.Core               as C
import           RSCoin.Explorer.Summaries (CoinsMapSummary, TransactionSummary)
import           RSCoin.Explorer.Web.Aeson ()

-- | Run-time errors which may happen within this server.
data ServerError
    = ParseError !Text
    | NotFound !Text
    deriving (Show,Generic)

$(deriveJSON defaultOptionsPS ''ServerError)

type ErrorableMsg msg = Either ServerError msg

-- | Communication starts with Introductory Message sent by
-- client. This type describes all such messages. Introductiory
-- message starts communication between server and client about some
-- topic (e. g. about particular address).
data IntroductoryMsg
    =
      -- | AddressInfo starts communication about given Address. Within
      -- this communication user can request various data about address.
      IMAddressInfo !C.Address
    |
      -- | Get transaction with this specific transaction Id
      IMTransactionInfo !C.TransactionId
    |
      -- | Get info either about AddressInfo or about TransactionInfo
      IMInfo !C.Address !C.TransactionId
    deriving (Show,Generic)

$(deriveJSON defaultOptionsPS ''IntroductoryMsg)

customDecode
    :: FromJSON a
    => BSL.ByteString -> Either ServerError a
customDecode = mapLeft (ParseError . pack) . eitherDecode

instance WS.WebSocketsData (ErrorableMsg IntroductoryMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize IntroductoryMsg is illegal"

-- | Within communication started with AddressInfo message client can
-- send messages defined by this type.
data AddressInfoMsg
    =
      -- | GetBalance message requests balance of address associated
      -- with connection.
      AIGetBalance
    |
      -- | GetTxNumber message requests number of transactions
      -- referencing address associated with connection.
      AIGetTxNumber
    |
      -- | GetTransactions message requests transactions referencing
      -- address associated with connection. Arguments (lo, hi)
      -- determine which subset to return, i. e. transactions with
      -- indices in range [lo, hi) are returned. For instance,
      -- `AIGetTransactions (0, 2)` requests two most recent
      -- transactions.
      AIGetTransactions !(Word, Word)
    |
      -- | Change user address TODO: improve this description
      AIChangeAddress !C.Address
    |
      -- | Get info either about AddressInfo or about TransactionInfo
      AIChangeInfo !C.Address !C.TransactionId
    deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''AddressInfoMsg)

instance WS.WebSocketsData (ErrorableMsg AddressInfoMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize AddressInfoMsg is illegal"

-- | This type contains all possible messages sent by this server.
data OutcomingMsg
    =
      -- | Sent in case of error.
      OMError !ServerError
    |
      -- | Sent within `AddressInfo` session.
      OMBalance !C.PeriodId
                !CoinsMapSummary
    |
      -- | Sent within `AddressInfo` session. Contains number of
      -- transactions referencing address over given PeriodId.
      OMTxNumber !C.PeriodId
                 !Text
    |
      -- | Sent within `TransactionInfo` session. Contains transaction
      -- that is requested with its ThransactionId.
      OMTransaction !TransactionSummary
    |
      -- | Sent within `AddressInfo` session. This is a confirmation
      -- sent on establishing session.
      OMSessionEstablished !C.Address
    |
      -- | Sent within `AddressInfo` session. Has an indexed list of
      -- transactions referencing address over given PeriodId.
      OMTransactions !C.PeriodId ![(Word, TransactionSummary)]
    deriving (Show,Generic)

$(deriveToJSON defaultOptionsPS ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = error "Attempt to deserialize OutcomingMsg is illegal"
    toLazyByteString = encode
