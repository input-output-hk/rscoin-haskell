module Bench.RSCoin.InfraThreads
        ( addMintette
        , bankThread
        , mintetteThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (update)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, defaultPort,
                                             readSecretKey)
import qualified RSCoin.Mintette            as M

import           RSCoin.Timed               (MsgPackRpc, bracket', fork,
                                             runRealMode)

import           System.FilePath            ((</>))

bankBracket :: FilePath -> (B.State -> MsgPackRpc a) -> IO a
bankBracket benchDir bankAction = runRealMode $ bracket'
    (liftIO $ B.openState $ benchDir </> "bank-db")
    (liftIO . B.closeState)
    bankAction

addMintette :: Int -> FilePath -> PublicKey -> IO ()
addMintette mintetteNum benchDir publicKey = bankBracket benchDir $ \bankState -> liftIO $ do
    let mintette = Mintette "127.0.0.1" (defaultPort + mintetteNum)
    update bankState $ B.AddMintette mintette publicKey

bankThread :: FilePath -> FilePath -> IO ()
bankThread benchDir bankKeyFilePath = bankBracket benchDir $ \bankState -> do
    secretKey <- liftIO $ readSecretKey bankKeyFilePath
    fork $ B.runWorker secretKey bankState
    B.serve bankState

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteNum benchDir secretKey = runRealMode $ bracket'
    (liftIO $ M.openState $ benchDir </> dbFormatPath "mintette-db" mintetteNum)
    (liftIO . M.closeState) $
    \mintetteState -> do
        fork $ M.runWorker secretKey mintetteState
        M.serve (defaultPort + mintetteNum) mintetteState secretKey