-- | Explorer Server.

module RSCoin.Explorer.Server
       ( serve
       ) where


import           Data.Acid.Advanced        (query', update')

import qualified RSCoin.Core               as C
import           RSCoin.Timed              (WorkMode)

import           RSCoin.Explorer.AcidState (AddHBlock (..),
                                            GetLastPeriodId (..), State)

-- TODO: we need encrypted communication, so this server is dummy for now.
serve
    :: WorkMode m
    => Int -> State -> C.SecretKey -> m ()
serve port _ _ = do
    C.serve port []
    () <$ handleNewHBlock undefined undefined undefined

handleNewHBlock
    :: WorkMode m
    => State -> C.PeriodId -> C.HBlock -> m C.PeriodId
handleNewHBlock st newBlockId newBlock = do
    expectedPid <- maybe 0 succ <$> query' st GetLastPeriodId
    if expectedPid == newBlockId
        then update' st (AddHBlock newBlockId newBlock) >> return newBlockId
        else return expectedPid
