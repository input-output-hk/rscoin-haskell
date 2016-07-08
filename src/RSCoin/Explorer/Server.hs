-- | Explorer Server.

module RSCoin.Explorer.Server
       ( serve
       ) where


import           Data.Acid.Advanced        (query', update')

import qualified RSCoin.Core               as C
import           RSCoin.Timed              (ServerT, WorkMode,
                                            serverTypeRestriction3)

import           RSCoin.Explorer.AcidState (AddHBlock (..),
                                            GetLastPeriodId (..), State)

serve
    :: WorkMode m
    => Int -> State -> C.SecretKey -> m ()
serve port st sk = do
    idr1 <- serverTypeRestriction3
    C.serve
        port
        [C.method (C.RSCExplorer C.EMNewBlock) $ idr1 $ handleNewHBlock st sk]

handleNewHBlock
    :: WorkMode m
    => State
    -> C.SecretKey
    -> C.PeriodId
    -> C.HBlock
    -> C.Signature
    -> ServerT m (C.PeriodId, C.Signature)
handleNewHBlock st sk newBlockId newBlock _ = do
    -- TODO: check sig
    expectedPid <- maybe 0 succ <$> query' st GetLastPeriodId
    let ret p = return (p, C.sign sk p)
    if expectedPid == newBlockId
        then update' st (AddHBlock newBlockId newBlock) >> ret (newBlockId + 1)
        else ret expectedPid
