{-# LANGUAGE TemplateHaskell #-}

-- | Storage containing whole bank's data.

module RSCoin.Bank.Storage.Storage
       (
         -- | Type
         Storage

         -- | Constructor
       , mkStorage

         -- | Lenses
       , mintettesStorage
       , explorersStorage
       , periodId
       , blocks
       , utxo
       , emissionHashes
       , addresses
       , pendingAddresses

       ) where

import           Control.Lens                  (makeLenses)

import qualified Data.Map                      as MP
import           Data.SafeCopy                 (base, deriveSafeCopy)
import           Data.Typeable                 (Typeable)

import qualified RSCoin.Core                   as C

import qualified RSCoin.Bank.Storage.Explorers as ES
import qualified RSCoin.Bank.Storage.Mintettes as MS


-- | Storage contains all the data used by Bank.
data Storage = Storage
    {
      -- | Data about mintettes.
      _mintettesStorage :: !MS.MintettesStorage
      -- | Data about explorers.
    , _explorersStorage :: !ES.ExplorersStorage
      -- | Id of ongoing period. Doesn't mean anything if there is no
      -- active period.
    , _periodId         :: !C.PeriodId
      -- | List of all blocks from the very beginning. Head of this
      -- list is the most recent block.
    , _blocks           :: ![C.HBlock]
      -- | Utxo for all the transaction ever made.
    , _utxo             :: !C.Utxo
      -- | List off all emission hashes from the very beginning.
    , _emissionHashes   :: ![C.TransactionId]
      -- | Known addresses accompanied with their strategies. Note that every address with
      -- non-default strategy should be stored here in order to participate in transaction.
    , _addresses        :: !C.AddressToTxStrategyMap
      -- | Pending addresses to publish within next HBlock.
    , _pendingAddresses :: !C.AddressToTxStrategyMap
    } deriving (Typeable)

$(makeLenses ''Storage)
$(deriveSafeCopy 0 'base ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage =
    Storage
    { _mintettesStorage = MS.mkMintettesStorage
    , _explorersStorage = ES.mkExplorersStorage
    , _periodId = 0
    , _blocks = []
    , _utxo = MP.empty
    , _emissionHashes = []
    , _addresses = MP.empty
    , _pendingAddresses = MP.empty
    }
