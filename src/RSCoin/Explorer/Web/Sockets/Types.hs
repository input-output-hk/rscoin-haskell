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
       , ControlMsg (..)
       , AddressInfoMsg (..)
       , HBlockInfoMsg (..)
       , IncomingMsg (..)
       , OutcomingMsg (..)
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
import           RSCoin.Explorer.Extended  (CoinsMapExtended, HBlockExtension,
                                            TransactionExtended)
import           RSCoin.Explorer.Web.Aeson ()

-- | Run-time errors which may happen within this server.
data ServerError
    = ParseError { peTypeName :: !Text
                 , peError    :: !Text}
    | NotFound !Text
    | LogicError !Text
    | LimitExceededError !Text
    deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''ServerError)

type ErrorableMsg msg = Either Text msg

-- | ControlMsg is used to manage context (or state) of connection. In
-- particular, server and client start communication with message of
-- this type.
data ControlMsg
     -- | SetAddress modifies context of connection setting address to
     -- the given one.  After that client can send `AddressInfoMsg` to
     -- get information about this address. Also client gets
     -- subscribed to notifications about this address.
    = CMSetAddress !C.Address
      -- | GetTransaction requests transaction with given id and
      -- unsubscribes client from all notifications, because
      -- information about transaction doesn't change (in our
      -- model). It shouldn't exist here, only in HTTP.
    | CMGetTransaction !C.TransactionId
      -- | This is special message which means that server has to
      -- determine whether given string is address or id of
      -- transaction and behave as CMGetTransaction or CMSetAddress.
    | CMSmart !Text
      -- | SetHBlock modifies context of connection setting HBlock to
      -- the given one. After that client can send `HBlockInfoMsg` and
      -- gets subscribed to notifications about new HBlocks.
    | CMSetHBlock !C.PeriodId
      -- | GetBlockchainHeight requests height of blockchain. It shouldn't exist
      -- here, only in HTTP.
    | CMGetBlockchainHeight
      -- | GetBlocksOverview requests overview (i. e. HBLockExtension)
      -- of some blocks. Indices are (lo, hi) and mean periods in
      -- range [lo, hi). It shouldn't exist here, only in HTTP.
    | CMGetBlocksOverview !(C.PeriodId, C.PeriodId)
      -- | GetRecentTransactions requests transactions from global
      -- history. Low indices mean the most recent transactions,
      -- interval is half-opened ([lo, hi)). Probably it will also be
      -- moved into HTTP.
    | CMGetTransactionsGlobal !(Word, Word)
      -- | SubscribeNewBlock makes client receive notifications about
      -- new blocks as soon as explorer receives them.
    | CMSubscribeNewBlocks
      -- | UnsubscribeNewBlock unsubscribes client from notifications
      -- about new blocks.
    | CMUnsubscribeNewBlocks
      -- | SubscribeAddress makes client receive notifications about
      -- updates related to given address.
    | CMSubscribeAddress !C.Address
      -- | UnsubscribeAddress unsubscribes client from notifications
      -- about updates related to address he is subscribed to.
    | CMUnsubscribeAddress
    deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''ControlMsg)

customDecode
    :: FromJSON a
    => BSL.ByteString -> Either Text a
customDecode = mapLeft pack . eitherDecode

instance WS.WebSocketsData (ErrorableMsg ControlMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize ControlMsg is illegal"

-- | After using SetAddress control message client can send messages
-- defined by this type.
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
    deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''AddressInfoMsg)

instance WS.WebSocketsData (ErrorableMsg AddressInfoMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize AddressInfoMsg is illegal"

-- | After using SetHBlock control message client can send messages
-- defined by this type.
data HBlockInfoMsg
     -- | GetMetadata message requests metadata of block associated
     -- with connection.
    = HIGetMetadata
      -- | GetTransactions message requests transactions stored in
      -- block associated with connection. Arguments (lo, hi) are
      -- indices, transactions with indices in range [lo, hi) are
      -- returned.
    | HIGetTransactions !(Word, Word)
    deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''HBlockInfoMsg)

-- | IncomingMsg aggregates all types of messages which server can receive.
data IncomingMsg
    = IMControl !ControlMsg
    | IMAddrInfo !AddressInfoMsg
    | IMHBlockInfo !HBlockInfoMsg
    deriving (Show,Generic)

$(deriveJSON defaultOptionsPS ''IncomingMsg)

instance WS.WebSocketsData (ErrorableMsg IncomingMsg) where
    fromLazyByteString = customDecode
    toLazyByteString = error "Attempt to serialize IncomingMsg is illegal"

-- | This type contains all possible messages sent by this server.
data OutcomingMsg
     -- | Sent in case of error.
    = OMError !ServerError
      -- | Sent within `AddressInfo` session.
    | OMBalance !C.Address
                !C.PeriodId
                !CoinsMapExtended
      -- | Sent within `AddressInfo` session. Contains number of
      -- transactions referencing address over given PeriodId.
    | OMTxNumber !C.Address
                 !C.PeriodId
                 !Word
      -- | Sent in response to CMGetTransaction message.
    | OMTransaction !C.TransactionId
                    !TransactionExtended
      -- | Sent within `AddressInfo` session. Has an indexed list of
      -- transactions referencing address over given PeriodId.
    | OMAddrTransactions !C.Address
                         !C.PeriodId
                         ![(Word, TransactionExtended)]
      -- | Sent within `HBlockInfo` session. Has metadata associated
      -- with block and index of block.
    | OMBlockMetadata !C.PeriodId
                      !HBlockExtension
      -- | Sent within `HBlockInfo` session. Has an indexed list of
      -- transactions in block, as well as index of block.
    | OMBlockTransactions !C.PeriodId
                          ![(Word, TransactionExtended)]
      -- | Sent in response to CMGetBlockchainHeight message.
    | OMBlockchainHeight !C.PeriodId
      -- | Sent in response to CMGetBlocksOverview message.
    | OMBlocksOverview ![(C.PeriodId, HBlockExtension)]
      -- | Signals about new HBlock with given id.
    | OMNewBlock !C.PeriodId
      -- | Sent in response to CMGetTransactionsGlobal message.
    | OMTransactionsGlobal !C.PeriodId
                           ![(Word, TransactionExtended)]
    deriving (Show, Generic)

$(deriveToJSON defaultOptionsPS ''OutcomingMsg)

instance WS.WebSocketsData OutcomingMsg where
    fromLazyByteString = error "Attempt to deserialize OutcomingMsg is illegal"
    toLazyByteString = encode
