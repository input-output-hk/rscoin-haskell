module Bench.RSCoin.Local.InfraThreads
        ( addMintette
        , bankThread
        , mintetteThread
        , notaryThread
        ) where

import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)

import           Data.Time.Units            (TimeUnit)

import           System.FilePath            ((</>))

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, defaultPort, localhost,
                                             testBankSecretKey)
import qualified RSCoin.Mintette            as M
import qualified RSCoin.Notary              as N
import           RSCoin.Timed               (fork, runRealModeUntrusted)

import           Bench.RSCoin.FilePathUtils (benchConfPath, dbFormatPath)

addMintette :: FilePath -> Int -> PublicKey -> IO ()
addMintette confPath mintetteId =
    B.addMintetteIO (Just confPath) testBankSecretKey mintette
  where
    mintette = Mintette localhost (defaultPort + mintetteId)

bankThread :: TimeUnit t => t -> FilePath -> IO ()
bankThread periodDelta benchDir =
    B.launchBankReal
        periodDelta
        (benchDir </> "bank-db")
        (Just $ benchConfPath benchDir)
        testBankSecretKey

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteId benchDir secretKey =
    runRealModeUntrusted (Just $ benchConfPath benchDir) $
    bracket
        (liftIO $
         M.openState $ benchDir </> dbFormatPath "mintette-db" mintetteId)
        (liftIO . M.closeState) $
    \mintetteState -> do
        _ <- fork $ M.runWorker secretKey mintetteState Nothing
        M.serve (defaultPort + mintetteId) mintetteState secretKey

notaryThread :: FilePath -> IO ()
notaryThread benchDir =
    runRealModeUntrusted (Just $ benchConfPath benchDir) $
    bracket
        (liftIO $ N.openState $ benchDir </> "notary-db")
        (liftIO . N.closeState)
        N.serveNotary
