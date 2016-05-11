module RSCoin.User.Action (Action (..)) where

import RSCoin.Core (Address)
import Data.Int    (Int64)

-- | Actions to be performed by ActionsExecutor
data Action = Exit
            | Send Address Int64
            | Update
