{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent        (ThreadId, forkIO, killThread)
import           Control.Monad             (unless)
import           Data.FileEmbed            (embedStringFile,
                                            makeRelativeToProject)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T (unlines)
import           Formatting                (build, int, sformat, text, (%))
import qualified Options.Generic           as OG
import qualified Turtle                    as T

import qualified RSCoin.Core               as C

import           Bench.RSCoin.Logging      (initBenchLogger, logInfo)
import           Bench.RSCoin.RemoteConfig (MintetteData (..),
                                            RemoteConfig (..), UsersData (..),
                                            readRemoteConfig)

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

exportCommand :: Word -> T.Text
exportCommand shardSize =
    sformat ("export " % text % "=" % int) C.shardSizeOption shardSize

bankSetupCommand :: Word -> [T.Text] -> [C.PublicKey] -> T.Text
bankSetupCommand shardSize mHosts mKeys =
    T.unlines
        [ cdCommand
        , exportCommand shardSize
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

bankRunCommand :: Word -> T.Text
bankRunCommand periodDelta =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-bank serve --log-severity Warning +RTS -qg -RTS --period-delta " %
               int)
              periodDelta]

bankStopCommand :: T.Text
bankStopCommand = "killall rscoin-bank"

mintetteKeyGenCommand :: Word -> T.Text
mintetteKeyGenCommand shardSize =
    T.unlines
        [ cdCommand
        , exportCommand shardSize
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , "git pull --ff-only"
        , "stack build rscoin"
        , "stack exec -- rscoin-keygen"]

mintetteCatKeyCommand :: T.Text
mintetteCatKeyCommand = "cat \"$HOME\"/.rscoin/key.pub\n"

mintetteRunCommand :: T.Text -> T.Text
mintetteRunCommand bankHost =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-mintette --log-severity Error +RTS -qg -RTS --bank-host " %
               build)
              bankHost]

mintetteStopCommand :: T.Text
mintetteStopCommand = "killall rscoin-mintette"

usersCommand :: Word -> T.Text -> Word -> Word -> T.Text
usersCommand shardSize bankHost u t =
    T.unlines
        [ cdCommand
        , exportCommand shardSize
        , "git pull --ff-only"
        , sformat
              ("stack bench rscoin:rscoin-bench-only-users --benchmark-arguments \"--users " %
               int %
               " --bank " %
               build %
               " --transactions " %
               int %
               " +RTS -qg\"")
              u
              bankHost
              t]

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

runBank :: Word -> Word -> T.Text -> [T.Text] -> [C.PublicKey] -> Bool -> IO ThreadId
runBank periodDelta shardSize bankHost mintetteHosts mintetteKeys hasRSCoin = do
    unless hasRSCoin $ installRSCoin bankHost
    runSsh bankHost $ bankSetupCommand shardSize mintetteHosts mintetteKeys
    forkIO $ runSsh bankHost $ bankRunCommand periodDelta

stopBank :: T.Text -> IO ()
stopBank bankHost = runSsh bankHost bankStopCommand

genMintetteKey :: Word -> T.Text -> IO C.PublicKey
genMintetteKey shardSize hostName = do
    runSsh hostName $ mintetteKeyGenCommand shardSize
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: T.Text -> MintetteData -> IO ThreadId
runMintette bankHost (MintetteData hasRSCoin hostName) = do
    unless hasRSCoin $ installRSCoin hostName
    forkIO $ runSsh hostName $ mintetteRunCommand bankHost

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsers :: Word -> T.Text -> UsersData -> Word -> Word -> IO ()
runUsers shardSize bankHost (UsersData hasRSCoin hostName) u t = do
    unless hasRSCoin $ installRSCoin hostName
    runSsh hostName $ usersCommand shardSize bankHost u t

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    RemoteConfig{..} <-
        readRemoteConfig $ fromMaybe "remote.yaml" $ rboConfigFile
    C.initLogging C.Error
    initBenchLogger $ fromMaybe C.Info $ rboBenchSeverity
    mintetteKeys <- mapM (genMintetteKey rcShard . mdHost) rcMintettes
    mintetteThreads <- mapM (runMintette rcBank) rcMintettes
    logInfo "Launched mintettes, waiting…"
    T.sleep 2
    logInfo "Launching bank…"
    bankThread <-
        runBank rcPeriod rcShard rcBank (map mdHost rcMintettes) mintetteKeys True
    logInfo "Launched bank, waiting…"
    T.sleep 3
    logInfo "Running users…"
    runUsers rcShard rcBank rcUsers rcUsersNum rcTransactionsNum
    logInfo "Ran users"
    killThread bankThread
    logInfo "Killed bank thread"
    stopBank rcBank
    logInfo "Stopped bank"
    mapM_ killThread mintetteThreads
    logInfo "Killed mintette threads"
    mapM_ stopMintette $ map mdHost rcMintettes
    logInfo "Stopped mintettes"
