module Bench.RSCoin.Local.InfraThreads
        ( addMintette
        , bankThread
        , mintetteThread
        , notaryThread
        ) where

import           Data.Optional              (Optional (Default))
import           Data.Time.Units            (TimeUnit)

import           System.FilePath            ((</>))

import qualified RSCoin.Bank                as B
import           RSCoin.Core                (Mintette (Mintette), PublicKey,
                                             SecretKey, Severity (Warning),
                                             defaultEpochDelta, defaultPort,
                                             localhost, testBankSecretKey)
import qualified RSCoin.Mintette            as M
import qualified RSCoin.Notary              as N
import           RSCoin.Timed               (ContextArgument (CADefault))

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

addMintette :: Int -> PublicKey -> IO ()
addMintette mintetteId =
    B.addMintetteReq CADefault testBankSecretKey mintette
  where
    mintette = Mintette localhost (defaultPort + mintetteId)

bankThread :: TimeUnit t => t -> FilePath -> IO ()
bankThread periodDelta benchDir =
    B.launchBankReal
        periodDelta
        (benchDir </> "bank-db")
        CADefault
        testBankSecretKey

mintetteThread :: Int -> FilePath -> SecretKey -> IO ()
mintetteThread mintetteId benchDir secretKey =
    M.launchMintetteReal defaultEpochDelta port secretKey dbPath CADefault
  where
    port = defaultPort + mintetteId
    dbPath = Just $ benchDir </> dbFormatPath "mintette-db" mintetteId

notaryThread :: FilePath -> IO ()
notaryThread benchDir =
    N.launchNotaryReal Warning dbPath B.CADefault webPort [] Default
  where
    webPort = defaultPort - 1
    dbPath = Just $ benchDir </> "notary-db"
