{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Applicative            (liftA2)
import           Control.Concurrent             (MVar, newMVar)
import           Control.Concurrent.MVar.Lifted (modifyMVar_)
import           Control.Lens                   ((^.))
import           Control.Monad                  (when)
import           Control.Monad.Catch            (catch, throwM)
import           Control.Monad.Trans            (lift, liftIO)

import           Data.Acid.Advanced             (query', update')
import           Data.List                      (nub, (\\))
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (catMaybes)
import           Formatting                     (build, int, sformat, (%))

import           Serokell.Util.Text             (listBuilderJSON, mapBuilder,
                                                 show')

import           RSCoin.Bank.AcidState          (AddExplorer (..),
                                                 AddMintette (..),
                                                 GetAddresses (..),
                                                 GetEmission (..),
                                                 GetExplorersAndPeriods (..),
                                                 GetHBlock (..), GetLogs (..),
                                                 GetMintettes (..),
                                                 GetPeriodId (..),
                                                 GetTransaction (..),
                                                 RemoveExplorer (..),
                                                 RemoveMintette (..), State)
import           RSCoin.Bank.Error              (BankError (BEInconsistentResponse))
import           RSCoin.Core                    (ActionLog,
                                                 AddressToTxStrategyMap,
                                                 Explorers, HBlock, MintetteId,
                                                 Mintettes, PeriodId, PublicKey,
                                                 Signature, Transaction,
                                                 TransactionId, logDebug,
                                                 logError, logInfo, verify)
import qualified RSCoin.Core.NodeConfig         as NC
import qualified RSCoin.Core.Protocol           as C
import qualified RSCoin.Core.Protocol.Types     as PT (BankLocalControlRequest (..),
                                                       checkLocalControlRequest)
import qualified RSCoin.Timed                   as T

serve
    :: T.WorkMode m
    => State -> T.ThreadId -> (T.ThreadId -> m T.ThreadId) -> m ()
serve st workerThread restartWorkerAction = do
    threadIdMVar <- liftIO $ newMVar workerThread
    idr1 <- T.serverTypeRestriction0
    idr2 <- T.serverTypeRestriction0
    idr3 <- T.serverTypeRestriction1
    idr4 <- T.serverTypeRestriction1
    idr5 <- T.serverTypeRestriction1
    idr6 <- T.serverTypeRestriction3
    idr7 <- T.serverTypeRestriction0
    idr8 <- T.serverTypeRestriction0
    idr9 <- T.serverTypeRestriction1
    idr10 <- T.serverTypeRestriction1

    (bankPublicKey, bankPort) <- liftA2 (,) (^. NC.bankPublicKey) (^. NC.bankPort)
                                 <$> T.getNodeContext

    C.serve
        bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ idr1 $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ idr2 $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlocks) $ idr3 $ serveGetHBlocks st
        , C.method (C.RSCBank C.GetTransaction) $ idr4 $ serveGetTransaction st
        , C.method (C.RSCBank C.FinishPeriod) $
          idr5 $ serveFinishPeriod st threadIdMVar restartWorkerAction bankPublicKey
        , C.method (C.RSCDump C.GetLogs) $ idr6 $ serveGetLogs st
        , C.method (C.RSCBank C.GetAddresses) $ idr7 $ serveGetAddresses st
        , C.method (C.RSCBank C.GetExplorers) $ idr8 $ serveGetExplorers st
        , C.method (C.RSCBank C.LocalControlRequest) $
          idr9 $ serveLocalControlRequest st bankPublicKey
        , C.method (C.RSCBank C.GetHBlockEmission) $
          idr10 $ serveGetHBlockEmission st]

toServer :: T.WorkMode m => m a -> T.ServerT m a
toServer action = lift $ action `catch` handler
  where
    handler (e :: BankError) = do
        logError $ show' e
        throwM e

-- toServer' :: T.WorkMode m => IO a -> T.ServerT m a
-- toServer' = toServer . liftIO

serveGetAddresses :: T.WorkMode m => State -> T.ServerT m AddressToTxStrategyMap
serveGetAddresses st =
    toServer $
    do mts <- query' st GetAddresses
       logDebug $
          sformat ("Getting list of addresses: " % build) $ mapBuilder $ M.toList mts
       return mts

serveGetMintettes :: T.WorkMode m => State -> T.ServerT m Mintettes
serveGetMintettes st =
    toServer $
    do mts <- query' st GetMintettes
       logDebug $ sformat ("Getting list of mintettes: " % build) mts
       return mts

serveGetHeight :: T.WorkMode m => State -> T.ServerT m PeriodId
serveGetHeight st =
    toServer $
    do pId <- query' st GetPeriodId
       logDebug $ sformat ("Getting blockchain height: " % build) pId
       return pId

serveGetHBlockEmission :: T.WorkMode m
               => State -> PeriodId -> T.ServerT m (Maybe (HBlock, Maybe TransactionId))
serveGetHBlockEmission st pId =
    toServer $
    do mBlock <- query' st (GetHBlock pId)
       emission <- query' st (GetEmission pId)
       logDebug $
           sformat ("Getting higher-level block with periodId " % build %
                    " and emission " % build % ": " % build)
                   pId emission mBlock
       return $ (,emission) <$> mBlock

serveGetHBlocks :: T.WorkMode m
                => State -> [PeriodId] -> T.ServerT m [HBlock]
serveGetHBlocks st (nub -> periodIds) =
    toServer $
    do logDebug $
           sformat ("Getting higher-level blocks in range: " % build) $
           listBuilderJSON periodIds
       blocks <-
           catMaybes <$>
           mapM (\pid -> fmap (, pid) <$> query' st (GetHBlock pid)) periodIds
       let gotIndices = map snd blocks
       when (gotIndices /= periodIds) $
           throwM $
           BEInconsistentResponse $
           sformat
               ("Couldn't get blocks for the following periods: " % build) $
           listBuilderJSON (periodIds \\ gotIndices)
       return $ map fst blocks

serveGetTransaction :: T.WorkMode m
                    => State -> TransactionId -> T.ServerT m (Maybe Transaction)
serveGetTransaction st tId =
    toServer $
    do t <- query' st (GetTransaction tId)
       logDebug $
           sformat ("Getting transaction with id " % build % ": " % build) tId t
       return t

-- !!! WARNING !!!
-- Usage of this function may accidentally lead to finishing period twice in a
-- row. But it is used only in benchmarks with infinite period so it is ok.
serveFinishPeriod
    :: T.WorkMode m
    => State
    -> MVar T.ThreadId
    -> (T.ThreadId -> m T.ThreadId)
    -> PublicKey
    -> Signature
    -> T.ServerT m ()
serveFinishPeriod st threadIdMVar restartAction bankPublicKey periodIdSignature = toServer $ do
    logInfo "Forced finish of period was requested"

    modifyMVar_ threadIdMVar $ \workerThreadId -> do
        currentPeriodId <- query' st GetPeriodId
        if verify bankPublicKey periodIdSignature currentPeriodId then
            restartAction workerThreadId
        else do
            logError $
                sformat ("Incorrect signature for periodId=" % int) currentPeriodId
            return workerThreadId

serveLocalControlRequest
    :: T.WorkMode m
    => State
    -> PublicKey
    -> PT.BankLocalControlRequest
    -> T.ServerT m (Maybe BankError)
serveLocalControlRequest st bankPublicKey controlRequest
  | not (PT.checkLocalControlRequest bankPublicKey controlRequest) =
      wrapResponse $ throwM $
      BEInconsistentResponse $
      sformat
          ("Tried to execute control request " % build %
           " with *invalid* signature")
          controlRequest
  | otherwise = wrapResponse $ do
      logInfo $ sformat ("Executing control request: " % build) controlRequest
      case controlRequest of
          PT.AddMintette m pk _         -> update' st (AddMintette m pk)
          PT.AddExplorer e pid _        -> update' st (AddExplorer e pid)
          PT.RemoveMintette host port _ -> update' st (RemoveMintette host port)
          PT.RemoveExplorer host port _ -> update' st (RemoveExplorer host port)
      logInfo $
          sformat
              ("Control request " % build % " executed successfully")
              controlRequest
  where
    wrapResponse action = lift $ (action >> return Nothing) `catch` handler
    handler (e :: BankError) = logError (show' e) >> return (Just e)

-- Dumping Bank state


serveGetLogs :: T.WorkMode m
             => State -> MintetteId -> Int -> Int -> T.ServerT m (Maybe ActionLog)
serveGetLogs st m from to =
    toServer $
    do mLogs <- query' st (GetLogs m from to)
       logDebug $
           sformat ("Getting action logs of mintette " % build %
                    " with range of entries " % int % " to " % int % ": " % build)
                   m from to mLogs
       return mLogs

serveGetExplorers
    :: T.WorkMode m
    => State -> T.ServerT m Explorers
serveGetExplorers st = do
    curPeriod <- query' st GetPeriodId
    map fst . filter ((== curPeriod) . snd) <$>
        query' st GetExplorersAndPeriods
