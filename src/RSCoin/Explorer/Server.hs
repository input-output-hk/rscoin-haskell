-- | Explorer Server.

module RSCoin.Explorer.Server
       ( serve
       ) where

import           Control.Exception         (throwIO)
import           Control.Lens              ((^.))
import           Control.Monad             (unless, when)
import           Control.Monad.Extra       (whenJust)
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Data.Acid                 (createCheckpoint)
import           Data.Acid.Advanced        (query', update')
import           Formatting                (build, int, sformat, (%))

import           Serokell.Util.AcidState   (createAndDiscardArchive)
import           Serokell.Util.Text        (listBuilderJSONIndent)

import qualified RSCoin.Core               as C
import           RSCoin.Timed              (MonadRpc (getNodeContext), ServerT,
                                            WorkMode, serverTypeRestriction3)

import           RSCoin.Explorer.AcidState (AddHBlock (..),
                                            GetLastPeriodId (..), State)
import           RSCoin.Explorer.Channel   (Channel, ChannelItem (..),
                                            writeChannel)
import           RSCoin.Explorer.Error     (ExplorerError (EEInvalidBankSignature))

serve
    :: WorkMode m
    => Int -> Channel -> State -> C.SecretKey -> Maybe FilePath -> m ()
serve port ch st _ storagePath = do
    idr1 <- serverTypeRestriction3
    bankPublicKey <- (^. C.bankPublicKey) <$> getNodeContext
    C.serve
        port
        [ C.method (C.RSCExplorer C.EMNewBlock) $
          idr1 $ handleNewHBlock ch st bankPublicKey storagePath]

handleNewHBlock
    :: WorkMode m
    => Channel
    -> State
    -> C.PublicKey
    -> Maybe FilePath
    -> C.PeriodId
    -> (C.HBlock, C.EmissionId)
    -> C.Signature
    -> ServerT m C.PeriodId
handleNewHBlock ch st bankPublicKey storagePath newBlockId (newBlock,emission) sig = do
    C.logInfo $ sformat ("Received new block #" % int) newBlockId
    unless (C.verify bankPublicKey sig (newBlockId, newBlock)) $
        liftIO $ throwIO EEInvalidBankSignature
    expectedPid <- maybe 0 succ <$> query' st GetLastPeriodId
    let ret p = do
            C.logDebug $ sformat ("Now expected block is #" % int) p
            return p
        upd = do
            C.logDebug $ sformat ("HBlock hash: " % build) (C.hash newBlock)
            C.logDebug $ sformat ("HBlock emission: " % build) emission
            C.logDebug $
                sformat ("Transaction hashes: " % build) $
                listBuilderJSONIndent 2 $
                map (C.hash :: C.Transaction -> C.Hash) $
                C.hbTransactions newBlock
            C.logDebug $
                sformat ("Transactions: " % build) $
                listBuilderJSONIndent 2 $ C.hbTransactions newBlock
            update' st (AddHBlock newBlockId newBlock emission)
            writeChannel
                ch
                ChannelItem
                { ciTransactions = C.hbTransactions newBlock
                }
            liftIO $ createCheckpoint st
            whenJust storagePath $
                \stpath ->
                     when (newBlockId `mod` 5 == 0) $
                     createAndDiscardArchive st stpath
            ret (newBlockId + 1)
    if expectedPid == newBlockId
        then upd
        else ret expectedPid
