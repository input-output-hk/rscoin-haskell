module Bench.RSCoin.Local.InfraThreads
        ( addMintette
        , bankThread
        , mintetteThread
        , notaryThread
        ) where

import           Control.Lens               ((^.))
import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)

import           Data.Maybe                 (fromJust)
import           Data.Time.Units            (TimeUnit)

import           System.FilePath            ((</>))

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette),
                                             NodeContext (..), PublicKey,
                                             SecretKey, bankSecretKey,
                                             defaultPort, localhost, localPlatformLayout)
import           RSCoin.Core.NodeConfig     (bankSecretKey)
import qualified RSCoin.Mintette            as M
import qualified RSCoin.Notary              as N
import           RSCoin.Timed               (fork, runRealModeLocal)

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

addMintette :: Int -> PublicKey -> IO ()
addMintette mintetteId = B.addMintetteIO (fromJust $ localPlatformLayout^.bankSecretKey) mintette
  where
    mintette = Mintette localhost (defaultPort + mintetteId)

bankThread :: (TimeUnit t) => t -> FilePath -> IO ()
bankThread periodDelta benchDir =
    B.launchBankReal
        localPlatformLayout
        periodDelta
        (benchDir </> "bank-db")
        (fromJust $ localPlatformLayout ^. bankSecretKey)

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteId benchDir secretKey =
    runRealModeLocal $
    bracket
        (liftIO $
         M.openState $ benchDir </> dbFormatPath "mintette-db" mintetteId)
        (liftIO . M.closeState) $
    \mintetteState ->
         do _ <- fork $ M.runWorker secretKey mintetteState
            M.serve (defaultPort + mintetteId) mintetteState secretKey

notaryThread :: FilePath -> IO ()
notaryThread benchDir =
    runRealModeLocal $
    bracket
        (liftIO $ N.openState $ benchDir </> "notary-db")
        (liftIO . N.closeState)
        (N.serve $ snd $ _notaryAddr localPlatformLayout)

