-- | Simple wrappers and re-exports on top of `RSCoin.User.Operations`.

module Test.RSCoin.Full.UserOperations
       ( getAllAddresses
       ) where

import           Control.Monad.Trans (MonadIO)
import           Data.Acid.Advanced  (query')

import           RSCoin.Core         (Address)
import qualified RSCoin.User         as U

getAllAddresses :: MonadIO m => U.RSCoinUserState -> m [Address]
getAllAddresses s = query' s U.GetPublicAddresses
