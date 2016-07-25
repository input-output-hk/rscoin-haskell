-- | Explorer Server.

module RSCoin.Explorer.Server
       ( serve
       ) where


import           Control.Exception         (throwIO)
import           Control.Monad             (unless)
import           Control.Monad.Trans       (MonadIO (liftIO))
import           Data.Acid.Advanced        (query', update')
import           Data.Text                 (Text)
import           Formatting                (int, sformat, (%))
import           Serokell.Util.Text        (formatSingle',
                                            listBuilderJSONIndent)


import qualified RSCoin.Core               as C
import           RSCoin.Timed              (ServerT, WorkMode,
                                            serverTypeRestriction3)

import           RSCoin.Explorer.AcidState (AddHBlock (..),
                                            GetLastPeriodId (..), State)
import           RSCoin.Explorer.Channel   (Channel, ChannelItem (..),
                                            writeChannel)
import           RSCoin.Explorer.Error     (ExplorerError (EEInvalidBankSignature))

logInfo, logDebug
    :: MonadIO m
    => Text -> m ()
logInfo = C.logInfo C.explorerLoggerName
logDebug = C.logDebug C.explorerLoggerName

serve
    :: WorkMode m
    => Int -> Channel -> State -> C.SecretKey -> m ()
serve port ch st _ = do
    idr1 <- serverTypeRestriction3
    C.serve
        port
        [C.method (C.RSCExplorer C.EMNewBlock) $ idr1 $ handleNewHBlock ch st]

handleNewHBlock
    :: WorkMode m
    => Channel
    -> State
    -> C.PeriodId
    -> (C.HBlock, C.EmissionId)
    -> C.Signature
    -> ServerT m C.PeriodId
handleNewHBlock ch st newBlockId (newBlock, emission) sig = do
    logInfo $ sformat ("Received new block #" % int) newBlockId
    unless (C.verify C.bankPublicKey sig (newBlockId, newBlock)) $
        liftIO $ throwIO EEInvalidBankSignature
    expectedPid <- maybe 0 succ <$> query' st GetLastPeriodId
    let ret p = do
            logDebug $ sformat ("Now expected block is #" % int) p
            return p
        upd = do
            logDebug $ formatSingle' "HBlock hash: {}" $ C.hash newBlock
            logDebug $ formatSingle' "HBlock emission: {}" emission
            logDebug $ formatSingle' "Transaction hashes: {}" $ listBuilderJSONIndent 2 $ map (C.hash :: C.Transaction -> C.Hash) $ C.hbTransactions newBlock
            logDebug $ formatSingle' "Transactions: {}" $ listBuilderJSONIndent 2 $ C.hbTransactions newBlock
            update' st (AddHBlock newBlockId newBlock emission)
            writeChannel
                ch
                ChannelItem
                { ciTransactions = C.hbTransactions newBlock
                }
            ret (newBlockId + 1)
    if expectedPid == newBlockId
        then upd
        else ret expectedPid
