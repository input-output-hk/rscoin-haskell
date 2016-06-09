module Bench.RSCoin.InfraThreads
        ( addMintette
        , bankThread
        , defaultBenchPeriod
        , mintetteThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (update)
import           Data.String                (IsString)
import           Data.Time.Units            (TimeUnit, Second)

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, bankSecretKey,
                                             defaultPort)
import qualified RSCoin.Mintette            as M

import           RSCoin.Timed               (MsgPackRpc, fork, runRealModeLocal)

import           System.FilePath            ((</>))

defaultBenchPeriod :: Second
defaultBenchPeriod = 7

localhost :: IsString s => s
localhost = "127.0.0.1"

bankBracket :: FilePath -> (B.State -> MsgPackRpc a) -> IO a
bankBracket benchDir bankAction =
    runRealModeLocal $
    bracket
        (liftIO $ B.openState $ benchDir </> "bank-db")
        (liftIO . B.closeState)
        bankAction

addMintette :: Int -> FilePath -> PublicKey -> IO ()
addMintette mintetteId benchDir publicKey =
    bankBracket benchDir $
    \bankState ->
         liftIO $
         do let mintette = Mintette localhost (defaultPort + mintetteId)
            update bankState $ B.AddMintette mintette publicKey

bankThread :: (TimeUnit t) => t -> FilePath -> IO ()
bankThread periodDelta benchDir
    = B.launchBank periodDelta (benchDir </> "bank-db") bankSecretKey

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
