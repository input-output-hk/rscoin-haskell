-- | Re-export functionality from RSCoin.User.* modules

module RSCoin.User
       (
         module Exports

       , getAllAddresses
       , A.RSCoinUserState
       ) where

import           Control.Monad.Trans    (MonadIO)

import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Cache      as Exports
import           RSCoin.User.Error      as Exports
import           RSCoin.User.Logic      as Exports
import           RSCoin.User.Operations (getAllPublicAddresses)
import           RSCoin.User.Operations as Exports hiding
                                                    (getAllPublicAddresses)
import           RSCoin.User.Wallet     as Exports hiding (UserAddress,
                                                    addAddress, getLastBlockId,
                                                    isInitialized,
                                                    makeUserAddress)

import qualified RSCoin.Core            as C

getAllAddresses
    :: MonadIO m
    => A.RSCoinUserState -> m [C.Address]
getAllAddresses = getAllPublicAddresses
