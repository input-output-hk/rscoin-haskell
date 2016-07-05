-- | Re-export functionality from RSCoin.User.* modules

module RSCoin.User
       (
         module Exports
       , getAllAddresses
       ) where

import           Control.Monad.Trans    (MonadIO)

import           Data.Acid.Advanced     (query')
import           RSCoin.User.AcidState  as Exports
import           RSCoin.User.Cache      as Exports
import           RSCoin.User.Error      as Exports
import           RSCoin.User.Logic      as Exports
import           RSCoin.User.Operations (getAllPublicAddresses)
import           RSCoin.User.Operations as Exports hiding
                                                    (getAllPublicAddresses)
import           RSCoin.User.Wallet     as Exports hiding (addAddress, getLastBlockId,
                                                    isInitialized)

import qualified RSCoin.Core            as C

getAllAddresses :: MonadIO m => RSCoinUserState -> m [C.Address]
getAllAddresses s = query' s GetOwnedDefaultAddresses
