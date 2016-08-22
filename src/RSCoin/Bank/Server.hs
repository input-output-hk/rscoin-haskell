{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Server implementation for Bank

module RSCoin.Bank.Server
       ( serve
       ) where

import           Control.Applicative        (liftA2)
import           Control.Lens               ((^.))
import           Control.Monad              (forM_, when)
import           Control.Monad.Catch        (SomeException, bracket_, catch,
                                             throwM)
import           Control.Monad.Trans        (lift, liftIO)

import           Data.IORef                 (IORef, atomicWriteIORef,
                                             modifyIORef, newIORef, readIORef)
import           Data.List                  (nub, (\\))
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe)
import qualified Data.Text                  as T
import           Formatting                 (build, int, sformat, stext, (%))

import           Serokell.Util.Bench        (measureTime_)
import           Serokell.Util.Text         (listBuilderJSON, mapBuilder, show')

import           RSCoin.Bank.AcidState      (AddAddress (..), AddExplorer (..),
                                             AddMintette (..),
                                             GetAddresses (..),
                                             GetEmission (..),
                                             GetExplorersAndPeriods (..),
                                             GetHBlock (..), GetHBlocks (..),
                                             GetLogs (..), GetMintettes (..),
                                             GetPeriodId (..),
                                             RemoveExplorer (..),
                                             RemoveMintette (..),
                                             RestoreExplorers (..),
                                             StartNewPeriod (..), State,
                                             getStatistics, query, tidyState,
                                             update)
import           RSCoin.Bank.Error          (BankError (BEInconsistentResponse))
import           RSCoin.Core                (ActionLog, AddressToTxStrategyMap,
                                             Explorers, HBlock, MintetteId,
                                             Mintettes, PeriodId, PublicKey,
                                             TransactionId, logDebug, logError,
                                             logInfo)
import qualified RSCoin.Core                as C
import qualified RSCoin.Core.NodeConfig     as NC
import qualified RSCoin.Core.Protocol.Types as PT (BankLocalControlRequest (..),
                                                   checkLocalControlRequest)
import qualified RSCoin.Timed               as T

serve
    :: T.WorkMode m
    => State -> IORef Bool -> m ()
serve st isPeriodChanging = do
    idr1 <- T.serverTypeRestriction0
    idr2 <- T.serverTypeRestriction0
    idr3 <- T.serverTypeRestriction1
    idr4 <- T.serverTypeRestriction3
    idr5 <- T.serverTypeRestriction0
    idr6 <- T.serverTypeRestriction0
    idr7 <- T.serverTypeRestriction1
    idr8 <- T.serverTypeRestriction1

    (bankPublicKey, bankPort) <- liftA2 (,) (^. NC.bankPublicKey) (^. NC.bankPort)
                                 <$> T.getNodeContext

    C.serve
        bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ idr1 $ serveGetMintettes st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ idr2 $ serveGetHeight st
        , C.method (C.RSCBank C.GetHBlocks) $ idr3 $ serveGetHBlocks st
        , C.method (C.RSCDump C.GetLogs) $ idr4 $ serveGetLogs st
        , C.method (C.RSCBank C.GetAddresses) $ idr5 $ serveGetAddresses st
        , C.method (C.RSCBank C.GetExplorers) $ idr6 $ serveGetExplorers st
        , C.method (C.RSCBank C.LocalControlRequest) $
          idr7 $ serveLocalControlRequest st bankPublicKey isPeriodChanging
        , C.method (C.RSCBank C.GetHBlockEmission) $
          idr8 $ serveGetHBlockEmission st]

type ServerTE m a = T.ServerT m (Either T.Text a)

toServer :: T.WorkMode m => m a -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: BankError) = do
        logError $ show' e
        return $ Left $ show' e

-- toServer' :: T.WorkMode m => IO a -> T.ServerT m a
-- toServer' = toServer . liftIO

serveGetAddresses
    :: T.WorkMode m
    => State -> ServerTE m AddressToTxStrategyMap
serveGetAddresses st =
    toServer $
    do mts <- query st GetAddresses
       logDebug $
          sformat ("Getting list of addresses: " % build) $ mapBuilder $ M.toList mts
       return mts

serveGetMintettes
    :: T.WorkMode m
    => State -> ServerTE m Mintettes
serveGetMintettes st =
    toServer $
    do mts <- query st GetMintettes
       logDebug $ sformat ("Getting list of mintettes: " % build) mts
       return mts

serveGetHeight :: T.WorkMode m => State -> ServerTE m PeriodId
serveGetHeight st =
    toServer $
    do pId <- query st GetPeriodId
       logDebug $ sformat ("Getting blockchain height: " % build) pId
       return pId

serveGetHBlockEmission
    :: T.WorkMode m
    => State -> PeriodId -> ServerTE m (Maybe (HBlock, Maybe TransactionId))
serveGetHBlockEmission st pId =
    toServer $
    do mBlock <- query st (GetHBlock pId)
       emission <- query st (GetEmission pId)
       logDebug $
           sformat ("Getting higher-level block with periodId " % build %
                    " and emission " % build % ": " % build)
                   pId emission mBlock
       return $ (,emission) <$> mBlock

serveGetHBlocks
    :: T.WorkMode m
    => State -> [PeriodId] -> ServerTE m [HBlock]
serveGetHBlocks st (nub -> periodIds) =
    toServer $
    do logDebug $
           sformat ("Getting higher-level blocks in range: " % build) $
           listBuilderJSON periodIds
       blocks <-
           catMaybes <$>
           mapM (\pid -> fmap (, pid) <$> query st (GetHBlock pid)) periodIds
       let gotIndices = map snd blocks
       when (gotIndices /= periodIds) $
           throwM $
           BEInconsistentResponse $
           sformat
               ("Couldn't get blocks for the following periods: " % build) $
           listBuilderJSON (periodIds \\ gotIndices)
       return $ map fst blocks

getPeriodResults
    :: T.WorkMode m
    => C.Mintettes -> C.PeriodId -> m [Maybe C.PeriodResult]
getPeriodResults mts pId = do
    res <- liftIO $ newIORef []
    mapM_ (f res) mts
    liftIO $ reverse <$> readIORef res
  where
    f res mintette =
        (C.sendPeriodFinished mintette pId >>=
         liftIO . modifyIORef res . (:) . Just) `catch`
        handler res
    handler res (e :: SomeException) =
        liftIO $
        do C.logWarning $ sformat
               ("Error occurred in communicating with mintette " % build) e
           modifyIORef res (Nothing :)

onPeriodFinished :: T.WorkMode m => C.SecretKey -> State -> m ()
onPeriodFinished sk st = do
    mintettes <- query st GetMintettes
    pId <- query st GetPeriodId
    C.logInfo $ sformat ("Period " % int % " has just finished!") pId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    initializeMultisignatureAddresses  -- init here to see them in next period
    periodResults    <- getPeriodResults mintettes pId
    (bankPk, genAdr) <- liftA2 (,) (^. C.bankPublicKey) (^. C.genesisAddress)
                        <$> T.getNodeContext
    newPeriodData    <- update st $ StartNewPeriod bankPk genAdr sk periodResults
    tidyState st
    newMintettes <- query st GetMintettes
    if null newMintettes
        then C.logWarning "New mintettes list is empty!"
        else do
            mapM_
                (\(m,mId) ->
                      C.announceNewPeriod m (newPeriodData !! mId) `catch`
                      handlerAnnouncePeriodM)
                (zip newMintettes [0 ..])
            C.logInfo $
                sformat
                    ("Announced new period with this NewPeriodData " %
                     "(payload is Nothing -- omitted (only in Debug)):\n" % build)
                    (C.formatNewPeriodData False $ head newPeriodData)
            C.logDebug $
                sformat
                    ("Announced new period, sent these newPeriodData's:\n" % build)
                    newPeriodData
    announceNewPeriodsToNotary `catch` handlerAnnouncePeriodsN
    update st RestoreExplorers
  where
    -- TODO: catch appropriate exception according to protocol implementation
    handlerAnnouncePeriodM (e :: SomeException) =
        C.logWarning $
        sformat ("Error occurred in communicating with mintette: " % build) e
    -- TODO: catch appropriate exception according to protocol implementation
    handlerAnnouncePeriodsN (e :: SomeException) =
        C.logWarning $
        sformat ("Error occurred in communicating with Notary: " % build) e
    initializeMultisignatureAddresses = do
        newMSAddresses <- C.queryNotaryCompleteMSAddresses
        forM_ newMSAddresses $ \(msAddr, strategy) -> do
            C.logInfo $ sformat ("Creating MS address " % build % " with strategy " % build)
                msAddr
                strategy
            update st $ AddAddress msAddr strategy
        C.logInfo "Removing new addresses from pool"
        mCurBankSecKey <- (^. NC.bankSecretKey) <$> T.getNodeContext
        let curBankSecKey  = fromMaybe
                                (error "Bank secret key is set to Nothing in config!")
                                mCurBankSecKey
        let msAddrs       = map fst newMSAddresses
        let signedMsAddrs = C.sign curBankSecKey msAddrs
        C.removeNotaryCompleteMSAddresses msAddrs signedMsAddrs
    announceNewPeriodsToNotary = do
        pId <- C.getNotaryPeriod
        pId' <- query st GetPeriodId
        C.announceNewPeriodsToNotary pId' =<< query st (GetHBlocks pId pId')

serveFinishPeriod
    :: T.WorkMode m
    => State
    -> IORef Bool
    -> m ()
serveFinishPeriod st isPeriodChanging = do
    let br =
            bracket_
                (liftIO $ atomicWriteIORef isPeriodChanging True)
                (liftIO $ atomicWriteIORef isPeriodChanging False)
    logInfo "Finish of period was requested"
    do bankSK <- (fromJust . (^. NC.bankSecretKey)) <$> T.getNodeContext
       t <- br $ measureTime_ $ onPeriodFinished bankSK st
       logInfo $ sformat ("Finishing period took " % build) t
       logDebug . sformat ("Storage statistics:\n" % stext) =<<
           getStatistics st

serveLocalControlRequest
    :: T.WorkMode m
    => State
    -> PublicKey
    -> IORef Bool
    -> PT.BankLocalControlRequest
    -> ServerTE m ()
serveLocalControlRequest st bankPublicKey isPeriodChanging controlRequest = do
    periodId <- query st GetPeriodId
    if not (PT.checkLocalControlRequest periodId bankPublicKey controlRequest) then
        toServer $ throwM $
        BEInconsistentResponse $
        sformat
            ("Tried to execute control request " % build %
             " with *invalid* signature")
            controlRequest
    else toServer $ do
        logInfo $ sformat ("Executing control request: " % build) controlRequest
        case controlRequest of
            PT.AddMintette m pk _         -> update st (AddMintette m pk)
            PT.AddExplorer e pid _        -> update st (AddExplorer e pid)
            PT.RemoveMintette host port _ -> update st (RemoveMintette host port)
            PT.RemoveExplorer host port _ -> update st (RemoveExplorer host port)
            PT.FinishPeriod _             -> serveFinishPeriod st isPeriodChanging
        logInfo $
            sformat
                ("Control request " % build % " executed successfully")
                controlRequest


-- Dumping Bank state

serveGetLogs
    :: T.WorkMode m
    => State -> MintetteId -> Int -> Int -> ServerTE m (Maybe ActionLog)
serveGetLogs st m from to =
    toServer $
    do mLogs <- query st (GetLogs m from to)
       logDebug $
           sformat ("Getting action logs of mintette " % build %
                    " with range of entries " % int % " to " % int % ": " % build)
                   m from to mLogs
       return mLogs

serveGetExplorers
    :: T.WorkMode m
    => State -> ServerTE m Explorers
serveGetExplorers st =
    toServer $
    do curPeriod <- query st GetPeriodId
       map fst . filter ((== curPeriod) . snd) <$>
           query st GetExplorersAndPeriods
