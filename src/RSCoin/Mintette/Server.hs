{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Server implementation for mintette

module RSCoin.Mintette.Server
       ( serve
       ) where

import           Control.Exception         (bracket, try)
import           Control.Monad.Trans       (lift)
import           Data.Acid.Advanced        (update')

import qualified RSCoin.Core               as C
import           RSCoin.Mintette.AcidState (CheckNotDoubleSpent (..),
                                            CommitTx (..), FinishPeriod (..),
                                            StartPeriod (..), State, closeState,
                                            openState)
import           RSCoin.Mintette.Error     (MintetteError)
import           RSCoin.Mintette.Worker    (runWorker)

serve :: Int -> FilePath -> C.SecretKey -> IO ()
serve port dbPath sk =
    bracket (openState dbPath) closeState $
    \st ->
         do runWorker sk st
            C.serve port
                [ C.method (C.RSCMintette C.PeriodFinished) $ handlePeriodFinished sk st
                , C.method (C.RSCMintette C.AnnounceNewPeriod) $ handleNewPeriod st
                , C.method (C.RSCMintette C.CheckTx) $ handleCheckTx sk st
                , C.method (C.RSCMintette C.CommitTx) $ handleCommitTx sk st
                ]

handlePeriodFinished
    :: C.SecretKey -> State -> C.PeriodId -> C.Server C.PeriodResult
handlePeriodFinished sk st pId = fmap C.AsMessagePack . update' st $ FinishPeriod sk pId

handleNewPeriod :: State
                -> C.AsMessagePack (C.MintetteId, C.NewPeriodData)
                -> C.Server ()
handleNewPeriod st (C.getAsMessagePack -> d) =
    fmap C.AsMessagePack . update' st $ StartPeriod d

handleCheckTx
    :: C.SecretKey
    -> State
    -> C.AsMessagePack C.Transaction
    -> C.AsMessagePack C.AddrId
    -> C.AsMessagePack C.Signature
    -> C.Server (Maybe C.CheckConfirmation)
handleCheckTx sk st (C.getAsMessagePack -> tx) (C.getAsMessagePack -> addrId) (C.getAsMessagePack -> sg) = lift . fmap C.AsMessagePack $ do
    (res :: Either MintetteError C.CheckConfirmation) <-
        try $ update' st $ CheckNotDoubleSpent sk tx addrId sg
    either (const $ return Nothing) (return . Just) res

handleCommitTx
    :: C.SecretKey
    -> State
    -> C.AsMessagePack C.Transaction
    -> C.PeriodId
    -> C.AsMessagePack C.CheckConfirmations
    -> C.Server (Maybe C.CommitConfirmation)
handleCommitTx sk st (C.getAsMessagePack -> tx) pId (C.getAsMessagePack -> cc) = lift . fmap C.AsMessagePack $ do
    (res :: Either MintetteError C.CommitConfirmation) <-
        try $ update' st $ CommitTx sk tx pId cc
    either (const $ return Nothing) (return . Just) res
