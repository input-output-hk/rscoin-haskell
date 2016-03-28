-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( CC.getBlockchainHeight
       , CC.getBlockByHeight
       ) where

import qualified RSCoin.Core.Communication as CC
