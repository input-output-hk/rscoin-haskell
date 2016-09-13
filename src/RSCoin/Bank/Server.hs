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

import           Data.Binary                (Binary)
import           Data.IORef                 (IORef, atomicWriteIORef,
                                             modifyIORef, newIORef, readIORef)
import           Data.List                  (nub, (\\))
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Formatting                 (build, int, sformat, stext, (%))

import           Serokell.Util.Bench        (measureTime_)
import           Serokell.Util.Text         (listBuilderJSON, show')

import qualified Control.TimeWarp.Rpc       as Rpc
import           RSCoin.Core                (Explorers, HBlock, Mintettes,
                                             PeriodId, PublicKey, SecretKey,
                                             getNodeContext, logDebug, logError,
                                             logInfo)
import qualified RSCoin.Core                as C
import qualified RSCoin.Core.NodeConfig     as NC
import qualified RSCoin.Core.Protocol.Types as PT (BankLocalControlRequest (..),
                                                   checkLocalControlRequest)

import           RSCoin.Bank.AcidState      (AddAddress (..), AddExplorer (..),
                                             AddMintette (..),
                                             CheckAndBumpStatisticsId (..),
                                             GetExplorersAndPeriods (..),
                                             GetHBlock (..), GetHBlocks (..),
                                             GetMintettes (..),
                                             GetPeriodId (..),
                                             GetStatisticsId (..),
                                             RemoveExplorer (..),
                                             RemoveMintette (..),
                                             RestoreExplorers (..),
                                             StartNewPeriod (..), State,
                                             getStatistics, query, tidyState,
                                             update)
import           RSCoin.Bank.Error          (BankError (BEInconsistentResponse))

serve
    :: C.WorkMode m
    => State -> SecretKey -> IORef Bool -> m ()
serve st bankSK isPeriodChanging = do
    idr1 <- Rpc.serverTypeRestriction0
    idr2 <- Rpc.serverTypeRestriction0
    idr3 <- Rpc.serverTypeRestriction0
    idr4 <- Rpc.serverTypeRestriction1
    -- idr5 <- Rpc.serverTypeRestriction0
    idr6 <- Rpc.serverTypeRestriction0
    idr7 <- Rpc.serverTypeRestriction1
    (bankPK,bankPort) <-
        liftA2 (,) (^. NC.bankPublicKey) (^. NC.bankPort) <$> getNodeContext
    C.serve
        bankPort
        [ C.method (C.RSCBank C.GetMintettes) $ idr1 $ serveGetMintettes bankSK st
        , C.method (C.RSCBank C.GetBlockchainHeight) $ idr2 $ serveGetHeight bankSK st
        , C.method (C.RSCBank C.GetStatisticsId) $
          idr3 $ serveGetStatisticsId bankSK st
        , C.method (C.RSCBank C.GetHBlocks) $ idr4 $ serveGetHBlocks bankSK st
        -- , C.method (C.RSCBank C.GetAddresses) $ idr5 $ serveGetAddresses bankSK st
        , C.method (C.RSCBank C.GetExplorers) $ idr6 $ serveGetExplorers bankSK st
        , C.method (C.RSCBank C.LocalControlRequest) $
          idr7 $ serveLocalControlRequest st bankPK bankSK isPeriodChanging]

type ServerTE m a = Rpc.ServerT m (Either T.Text a)

type ServerTESigned m a = ServerTE m (C.WithSignature a)

toServer :: C.WorkMode m => m a -> ServerTE m a
toServer action = lift $ (Right <$> action) `catch` handler
  where
    handler (e :: BankError) = do
        logError $ show' e
        return $ Left $ show' e

signHandler
    :: (Binary a, Functor m)
    => C.SecretKey -> ServerTE m a -> ServerTESigned m a
signHandler sk = fmap (fmap (C.mkWithSignature sk))

toServerSigned
    :: (C.WorkMode m, Binary a)
    => C.SecretKey -> m a -> ServerTESigned m a
toServerSigned sk = signHandler sk . toServer

-- serveGetAddresses
--     :: C.WorkMode m
--     => C.SecretKey -> State -> ServerTESigned m AddressToTxStrategyMap
-- serveGetAddresses sk st =
--     toServerSigned sk $
--     do mts <- query st GetAddresses
--        logDebug $
--           sformat ("Getting list of addresses: " % build) $ mapBuilder $ M.toList mts
--        return mts

serveGetMintettes
    :: C.WorkMode m
    => C.SecretKey -> State -> ServerTESigned m Mintettes
serveGetMintettes sk st =
    toServerSigned sk $
    do mts <- query st GetMintettes
       logDebug $ sformat ("Getting list of mintettes: " % build) mts
       return mts

serveGetHeight
    :: C.WorkMode m
    => C.SecretKey -> State -> ServerTESigned m PeriodId
serveGetHeight sk st =
    toServerSigned sk $
    do pId <- query st GetPeriodId
       logDebug $ sformat ("Getting blockchain height: " % build) pId
       return pId

serveGetStatisticsId
    :: C.WorkMode m
    => C.SecretKey -> State -> ServerTESigned m PeriodId
serveGetStatisticsId sk st = toServerSigned sk $ query st GetStatisticsId

serveGetHBlocks
    :: C.WorkMode m
    => C.SecretKey -> State -> [PeriodId] -> ServerTESigned m [HBlock]
serveGetHBlocks sk st (nub -> periodIds) =
    toServerSigned sk $
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
    :: C.WorkMode m
    => C.SecretKey -> C.Mintettes -> C.PeriodId -> m [Maybe C.PeriodResult]
getPeriodResults sk mts pId = do
    res <- liftIO $ newIORef []
    mapM_ (f res) mts
    liftIO $ reverse <$> readIORef res
  where
    f res mintette =
        (C.sendPeriodFinished mintette sk pId >>=
         liftIO . modifyIORef res . (:) . Just) `catch`
        handler res
    handler res (e :: SomeException) =
        liftIO $
        do C.logWarning $ sformat
               ("Error occurred in communicating with mintette " % build) e
           modifyIORef res (Nothing :)

onPeriodFinished :: C.WorkMode m => SecretKey -> State -> m ()
onPeriodFinished sk st = do
    mintettes <- query st GetMintettes
    pId <- query st GetPeriodId
    C.logInfo $ sformat ("Period " % int % " has just finished!") pId
    -- Mintettes list is empty before the first period, so we'll simply
    -- get [] here in this case (and it's fine).
    initializeMultisignatureAddresses  -- init here to see them in next period
    periodResults <- getPeriodResults sk mintettes pId
    timestamp <- liftIO getPOSIXTime
    newPeriodData <- update st $ StartNewPeriod timestamp sk periodResults
    tidyState st
    newMintettes <- query st GetMintettes
    if null newMintettes
        then C.logWarning "New mintettes list is empty!"
        else do
            mapM_
                (\(m,mId) ->
                      C.announceNewPeriod m sk (newPeriodData !! mId) `catch`
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
        let msAddrs       = map fst newMSAddresses
        let signedMsAddrs = C.sign sk msAddrs
        C.removeNotaryCompleteMSAddresses msAddrs signedMsAddrs
    announceNewPeriodsToNotary = do
        pId     <- C.getNotaryPeriod
        pId'    <- query st GetPeriodId
        hblocks <- query st (GetHBlocks pId pId')
        C.announceNewPeriodsToNotary sk pId' hblocks

serveFinishPeriod
    :: C.WorkMode m
    => State
    -> SecretKey
    -> IORef Bool
    -> m ()
serveFinishPeriod st bankSK isPeriodChanging = do
    let br =
            bracket_
                (liftIO $ atomicWriteIORef isPeriodChanging True)
                (liftIO $ atomicWriteIORef isPeriodChanging False)
    logInfo "Finish of period was requested"
    t <- br $ measureTime_ $ onPeriodFinished bankSK st
    logInfo $ sformat ("Finishing period took " % build) t

serveDumpStatistics
    :: C.WorkMode m
    => State -> Int -> m ()
serveDumpStatistics st sId = do
    good <- update st $ CheckAndBumpStatisticsId sId
    when good $
        do pId <- query st GetPeriodId
           liftIO . TIO.putStrLn . sformat
                   ("Storage statistics (period id is " % int % "):\n" % stext)
                   pId =<< getStatistics st

serveLocalControlRequest
    :: C.WorkMode m
    => State
    -> PublicKey
    -> SecretKey
    -> IORef Bool
    -> PT.BankLocalControlRequest
    -> ServerTE m ()
serveLocalControlRequest st bankPK bankSK isPeriodChanging controlRequest = do
    periodId <- query st GetPeriodId
    if not (PT.checkLocalControlRequest periodId bankPK controlRequest) then
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
            PT.FinishPeriod _             -> serveFinishPeriod st bankSK isPeriodChanging
            PT.DumpStatistics sId _       -> serveDumpStatistics st sId
        logInfo $
            sformat
                ("Control request " % build % " executed successfully")
                controlRequest


serveGetExplorers
    :: C.WorkMode m
    => C.SecretKey -> State -> ServerTESigned m Explorers
serveGetExplorers sk st =
    toServerSigned sk $
    do curPeriod <- query st GetPeriodId
       map fst . filter ((== curPeriod) . snd) <$>
           query st GetExplorersAndPeriods
