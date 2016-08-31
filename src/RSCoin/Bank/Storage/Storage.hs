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
       , addressesStorage
       , periodId
       , blocks
       , utxo
       , statisticsId

       ) where

import           Control.Lens                  (makeLenses)
import qualified Data.Map                      as MP
import           Data.SafeCopy                 (base, deriveSafeCopy)
import           Data.Typeable                 (Typeable)

import qualified RSCoin.Core                   as C

import qualified RSCoin.Bank.Storage.Addresses as AS
import qualified RSCoin.Bank.Storage.Explorers as ES
import qualified RSCoin.Bank.Storage.Mintettes as MS
import           RSCoin.Bank.Types             (HBlockMetadata)

-- | Storage contains all the data used by Bank.
data Storage = Storage
    {
      -- | Data about mintettes.
      _mintettesStorage :: !MS.MintettesStorage
      -- | Data about explorers.
    , _explorersStorage :: !ES.ExplorersStorage
      -- | Data about addresses.
    , _addressesStorage :: !AS.AddressesStorage
      -- | Id of ongoing period. Doesn't mean anything if there is no
      -- active period.
    , _periodId         :: !C.PeriodId
      -- | List of all blocks from the very beginning. Head of this
      -- list is the most recent block.
    , _blocks           :: ![C.WithMetadata C.HBlock HBlockMetadata]
      -- | Utxo for all the transaction ever made.
    , _utxo             :: !C.Utxo
      -- | Used to make `DumpStatistics` endpoint private.
    , _statisticsId     :: !Int
    } deriving (Typeable)

$(makeLenses ''Storage)
$(deriveSafeCopy 0 'base ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage =
    Storage
    { _mintettesStorage = MS.mkMintettesStorage
    , _explorersStorage = ES.mkExplorersStorage
    , _addressesStorage = AS.mkAddressesStorage
    , _periodId = 0
    , _blocks = []
    , _utxo = MP.empty
    , _statisticsId = 42
    }
