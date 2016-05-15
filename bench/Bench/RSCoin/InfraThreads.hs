module Bench.RSCoin.InfraThreads
        ( addMintette
        , bankThread
        , mintetteThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (update)
import           Data.Time.Units            (Second)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, bankHost, bankSecretKey,
                                             defaultPort)
import qualified RSCoin.Mintette            as M

import           RSCoin.Timed               (MsgPackRpc, fork, runRealMode)

import           System.FilePath            ((</>))

bankBracket :: FilePath -> (B.State -> MsgPackRpc a) -> IO a
bankBracket benchDir bankAction = runRealMode $ bracket
    (liftIO $ B.openState $ benchDir </> "bank-db")
    (liftIO . B.closeState)
    bankAction

addMintette :: Int -> FilePath -> PublicKey -> IO ()
addMintette mintetteNum benchDir publicKey = bankBracket benchDir $ \bankState -> liftIO $ do
    let mintette = Mintette bankHost (defaultPort + mintetteNum)
    update bankState $ B.AddMintette mintette publicKey

bankThread :: FilePath -> IO ()
bankThread benchDir = bankBracket benchDir $ \bankState -> do
    let defaultBenchPeriod = 5 :: Second
    _ <- fork $ B.runWorkerWithPeriod defaultBenchPeriod bankSecretKey bankState
    B.serve bankState

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteNum benchDir secretKey = runRealMode $ bracket
    (liftIO $ M.openState $ benchDir </> dbFormatPath "mintette-db" mintetteNum)
    (liftIO . M.closeState) $
    \mintetteState -> do
        _ <- fork $ M.runWorker secretKey mintetteState
        M.serve (defaultPort + mintetteNum) mintetteState secretKey