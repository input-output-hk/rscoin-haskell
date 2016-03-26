-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           RSCoin.Bank.AcidState (State)

serve :: Int -> State -> IO ()
serve = undefined
