{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent        (ThreadId, forkIO, killThread)
import           Control.Monad             (unless)
import           Data.FileEmbed            (embedStringFile,
                                            makeRelativeToProject)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T (unlines)
import           Formatting                (build, int, sformat, (%))
import qualified Options.Generic           as OG
import qualified Turtle                    as T

import qualified RSCoin.Core               as C

import           Bench.RSCoin.Logging      (initBenchLogger, logInfo)
import           Bench.RSCoin.RemoteConfig (MintetteData (..),
                                            RemoteConfig (..), readRemoteConfig)

data RemoteBenchOptions = RemoteBenchOptions
    { rboConfigFile    :: Maybe FilePath
    , rboBenchSeverity :: Maybe C.Severity
    } deriving (Show, OG.Generic)

instance OG.ParseField  C.Severity
instance OG.ParseFields C.Severity
instance OG.ParseRecord C.Severity
instance OG.ParseRecord RemoteBenchOptions

userName :: T.IsString s => s
userName = "ubuntu"

sshKeyPath :: T.IsString s => s
sshKeyPath = "~/.ssh/rscointest.pem"

installCommand :: T.IsString s => s
installCommand = $(makeRelativeToProject "bench/install.sh" >>= embedStringFile)

cdCommand :: T.Text
cdCommand = "cd \"$HOME/rscoin\""

bankSetupCommand :: [T.Text] -> [C.PublicKey] -> T.Text
bankSetupCommand mHosts mKeys =
    T.unlines
        [ cdCommand
        , bankStopCommand
        , "rm -rf bank-db"
        , "git pull --ff-only"
        , "stack build rscoin"
        , mconcat $ map (uncurry addMintetteCommand) $ zip mHosts mKeys]
  where
    addMintetteCommand =
        sformat
            ("stack exec -- rscoin-bank add-mintette --port " % int %
             " --host " %
             build %
             " --key " %
             build)
            (C.defaultPort :: Int)

bankRunCommand :: T.Text
bankRunCommand =
    T.unlines
        [ cdCommand
        , "stack exec -- rscoin-bank serve --log-severity Warning +RTS -qg -RTS"]

bankStopCommand :: T.Text
bankStopCommand = "killall rscoin-bank"

mintetteKeyGenCommand :: T.Text
mintetteKeyGenCommand =
    T.unlines
        [ cdCommand
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , "git pull --ff-only"
        , "stack build rscoin"
        , "stack exec -- rscoin-keygen"]

mintetteCatKeyCommand :: T.Text
mintetteCatKeyCommand = "cat \"$HOME\"/.rscoin/key.pub\n"

mintetteRunCommand :: T.Text
mintetteRunCommand =
    T.unlines
        [ cdCommand
        , "stack exec -- rscoin-mintette --log-severity Error +RTS -qg-RTS"]

mintetteStopCommand :: T.Text
mintetteStopCommand = "killall rscoin-mintette"

usersCommand :: Word -> Word -> T.Text
usersCommand u t =
    T.unlines
        [ cdCommand
        , "git pull --ff-only"
        , sformat
              ("stack bench rscoin:rscoin-bench-only-users --benchmark-arguments \"--users " %
               int %
               " --transactions " %
               int %
               " +RTS -qg\"")
              u t]

runSsh :: T.Text -> T.Text -> IO ()
runSsh hostName command = do
    T.ExitSuccess <-
        T.proc
            "ssh"
            ["-i", sshKeyPath, mconcat [userName, "@", hostName], command]
            mempty
    return ()

runSshStrict :: T.Text -> T.Text -> IO T.Text
runSshStrict hostName command = do
    (T.ExitSuccess, res) <-
        T.procStrict
            "ssh"
            ["-i", sshKeyPath, mconcat [userName, "@", hostName], command]
            mempty
    return res

installRSCoin :: T.Text -> IO ()
installRSCoin = flip runSsh installCommand

runBank :: [T.Text] -> [C.PublicKey] -> Bool -> IO ThreadId
runBank mintetteHosts mintetteKeys hasRSCoin = do
    unless hasRSCoin $ installRSCoin C.bankHost
    runSsh C.bankHost $ bankSetupCommand mintetteHosts mintetteKeys
    forkIO $ runSsh C.bankHost bankRunCommand

stopBank :: IO ()
stopBank = runSsh C.bankHost bankStopCommand

genMintetteKey :: T.Text -> IO C.PublicKey
genMintetteKey hostName = do
    runSsh hostName mintetteKeyGenCommand
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: MintetteData -> IO ThreadId
runMintette (MintetteData hasRSCoin hostName) = do
    unless hasRSCoin $ installRSCoin hostName
    forkIO $ runSsh hostName mintetteRunCommand

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsers :: (Bool, T.Text) -> Word -> Word -> IO ()
runUsers (hasRSCoin,hostName) u t = do
    unless hasRSCoin $ installRSCoin hostName
    runSsh hostName $ usersCommand u t

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    RemoteConfig{..} <-
        readRemoteConfig $ fromMaybe "remote.yaml" $ rboConfigFile
    C.initLogging C.Error
    initBenchLogger $ fromMaybe C.Info $ rboBenchSeverity
    mintetteKeys <- mapM (genMintetteKey . mdHost) rcMintettes
    mintetteThreads <- mapM runMintette rcMintettes
    logInfo "Launched mintettes, waiting…"
    T.sleep 2
    logInfo "Launching bank…"
    bankThread <- runBank (map mdHost rcMintettes) mintetteKeys True
    logInfo "Launched bank, waiting…"
    T.sleep 3
    logInfo "Running users…"
    runUsers users rcUsersNum rcTransactionsNum
    logInfo "Ran users"
    killThread bankThread
    logInfo "Killed bank thread"
    stopBank
    logInfo "Stopped bank"
    mapM_ killThread mintetteThreads
    logInfo "Killed mintette threads"
    mapM_ stopMintette $ map mdHost rcMintettes
    logInfo "Stopped mintettes"
  where
    users = (True, "52.28.239.209")
