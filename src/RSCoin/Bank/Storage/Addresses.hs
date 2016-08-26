{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage which contains data about explorers.

module RSCoin.Bank.Storage.Addresses
       ( AddressesStorage
       , mkAddressesStorage

       , getAddresses
       , addAddress
       , updateAddresses
       ) where

import           Control.Lens        (Getter, makeLenses, use, (%=), (.=))
import           Control.Monad       (unless)
import           Control.Monad.State (State)
import qualified Data.Map            as M
import           Data.SafeCopy       (base, deriveSafeCopy)
import           Data.Typeable       (Typeable)

import qualified RSCoin.Core         as C

data AddressesStorage = AddressesStorage
    { -- | Known addresses accompanied with their strategies. Note
      -- that every address with non-default strategy should be stored
      -- here in order to participate in transaction.
      _asAddresses        :: !C.AddressToTxStrategyMap
      -- | Pending addresses to publish within next HBlock.
    , _asPendingAddresses :: !C.AddressToTxStrategyMap
    } deriving (Typeable)

$(makeLenses ''AddressesStorage)
$(deriveSafeCopy 0 'base ''AddressesStorage)

mkAddressesStorage :: AddressesStorage
mkAddressesStorage =
    AddressesStorage
    { _asAddresses = M.empty
    , _asPendingAddresses = M.empty
    }

type Query a = Getter AddressesStorage a

-- | Returns addresses (to strategies) map
getAddresses :: Query C.AddressToTxStrategyMap
getAddresses = asAddresses

type Update a = State AddressesStorage a

-- | Add given address to storage and associate given strategy with it.
-- @TODO: Mind behaviour when address is being added more than once per period
addAddress :: C.Address -> C.TxStrategy -> Update ()
addAddress addr strategy = do
    curAddresses <- use asAddresses
    unless (addr `M.member` curAddresses) $
        asPendingAddresses %= M.insert addr strategy

-- | Add pending addresses to addresses map (addr -> strategy), return
-- it as an argument
updateAddresses :: Update C.AddressToTxStrategyMap
updateAddresses = do
    oldKnown <- use asAddresses
    pending <- use asPendingAddresses
    asAddresses %= flip M.union pending
    asPendingAddresses .= M.empty
    return $ pending M.\\ oldKnown
