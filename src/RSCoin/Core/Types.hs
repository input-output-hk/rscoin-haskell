-- | More complex types from the paper.

module RSCoin.Core.Types
       ( Mintette (..)
       , Mintettes
       , MintetteId
       , CheckConfirmation (..)
       , CheckConfirmations
       , PeriodId
       ) where

import qualified Data.Map               as M

import           RSCoin.Core.Crypto     (PublicKey, Signature)
import           RSCoin.Core.Primitives (AddrId)

-- | All the information about a particular mintette (WIP).
data Mintette = Mintette
    { mintetteHost :: String
    }

-- | Mintettes list is stored by Bank and doesn't change over period.
type Mintettes = [Mintette]

-- | Mintette is identified by it's index in mintettes list stored by Bank.
-- This id doesn't change over period, but may change between periods.
type MintetteId = Int

-- | CheckConfirmation is a confirmation received by user from mintette as
-- a result of CheckNotDoubleSpent action.
data CheckConfirmation = CheckConfirmation
    { ccMintetteKey       :: !PublicKey  -- ^ key of corresponding mintette
    , ccMintetteSignature :: !Signature  -- ^ signature for (tx, addrid)
    }

-- | CheckConfirmations is a bundle of evidence collected by user and
-- sent to mintette as payload for Commit action.
type CheckConfirmations = M.Map (MintetteId, AddrId) CheckConfirmation

-- | Periods are indexed by sequence of numbers starting from 0
type PeriodId = Int
