{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forM_, replicateM, void)
import           Formatting                 (build, sformat, (%))

-- workaround to make stylish-haskell work :(
import           Options.Generic

import           System.IO.Temp             (withSystemTempDirectory)

import           RSCoin.Core                (PublicKey, SecretKey,
                                             Severity (..), initLogging, keyGen)

import           Bench.RSCoin.FilePathUtils (tempBenchDirectory)
import           Bench.RSCoin.InfraThreads  (addMintette, bankThread,
                                             mintetteThread)
import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)

import           Turtle                     (empty, shell)

data BankBenchOptions = BankBenchOptions
    { mintettes     :: Int            <?> "number of mintettes"
    , severity      :: Maybe Severity <?> "severity for global logger"
    , benchSeverity :: Maybe Severity <?> "severity for bench logger"
    } deriving (Generic, Show)

instance ParseField  Severity
instance ParseFields Severity
instance ParseRecord Severity
instance ParseRecord BankBenchOptions

type KeyPairList = [(SecretKey, PublicKey)]

generateMintetteKeys :: Int -> IO KeyPairList
generateMintetteKeys n = replicateM n keyGen

runMintettes :: FilePath -> KeyPairList -> IO ()
runMintettes benchDir secretKeys
    = forM_ (zip [1..] secretKeys) $ \(mintetteId, (secretKey, publicKey)) -> do
        addMintette mintetteId benchDir publicKey
        void $ forkIO $ mintetteThread mintetteId benchDir secretKey

establishMintettes :: FilePath -> Int -> IO ()
establishMintettes benchDir mintettesNumber = do
    keyPairs <- generateMintetteKeys mintettesNumber
    runMintettes benchDir keyPairs
    logInfo $ sformat ("Running " % build % " mintettes…") mintettesNumber
    threadDelay $ 5 * 10 ^ (6 :: Int)

establishBank :: FilePath -> IO ()
establishBank benchDir = do
    _ <- forkIO $ bankThread benchDir
    logInfo "Running bank..."
    threadDelay $ 3 * 10 ^ (6 :: Int)

main :: IO ()
main = do
    BankBenchOptions{..} <- getRecord "rscoin-bench-bank"
    let mintettesNumber   = unHelpful mintettes
    let globalSeverity    = maybe Error id $ unHelpful severity
    let bSeverity         = maybe Info  id $ unHelpful benchSeverity
    withSystemTempDirectory tempBenchDirectory $ \benchDir -> do
        initLogging globalSeverity
        initBenchLogger bSeverity

        establishMintettes benchDir mintettesNumber
        establishBank      benchDir

        void $ shell "stack exec rscoin-bench-users -- --users 2" empty
