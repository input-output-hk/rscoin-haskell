-- | Channel used for communication between entities within Explorer.

module RSCoin.Explorer.Channel
       ( ChannelItem (..)
       , Channel
       , newChannel
       , newDummyChannel
       , readChannel
       , writeChannel
       ) where

import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Monad.Trans     (MonadIO (liftIO))

import qualified RSCoin.Core             as C

data ChannelItem = ChannelItem
    { ciTransactions :: ![C.Transaction]
    } deriving (Show)

data Channel
    = Channel (Chan ChannelItem)
    | DummyChannel

newChannel
    :: MonadIO m
    => m Channel
newChannel = Channel <$> liftIO newChan

newDummyChannel
    :: MonadIO m
    => m Channel
newDummyChannel = pure DummyChannel

readChannel
    :: MonadIO m
    => Channel -> m ChannelItem
readChannel (Channel ch) = liftIO $ readChan ch
readChannel DummyChannel =
    error "Attempt to use readChannel on DummyChannel is illegal"

writeChannel
    :: MonadIO m
    => Channel -> ChannelItem -> m ()
writeChannel (Channel ch) = liftIO . writeChan ch
writeChannel DummyChannel = const $ return ()
