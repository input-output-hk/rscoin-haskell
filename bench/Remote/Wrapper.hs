{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent         (ThreadId, forkIO, killThread)
import           Control.Monad              (unless)
import           Data.FileEmbed             (embedStringFile,
                                             makeRelativeToProject)
import           Data.List                  (genericTake)
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text                  as T (unlines)
import           Formatting                 (build, int, sformat, stext, (%))
import qualified Options.Generic            as OG
import qualified Turtle                     as T

import           Serokell.Util              (listBuilderJSON)

import qualified RSCoin.Core                as C

import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.Remote.Config (BankData (..), MintetteData (..),
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

data ShardParams = ShardParams
    { spDivider :: !Word
    , spDelta   :: !Word
    } deriving (Show)

data BankParams = BankParams
    { bpPeriodDelta :: !Word
    , bpShardParams :: !ShardParams
    , bpProfiling   :: !Bool
    , bpHasRSCoin   :: !Bool
    } deriving (Show)

userName :: T.IsString s => s
userName = "ubuntu"

sshKeyPath :: T.IsString s => s
sshKeyPath = "~/.ssh/rscointest.pem"

installCommand :: T.IsString s => s
installCommand = $(makeRelativeToProject "bench/install.sh" >>= embedStringFile)

cdCommand :: T.Text
cdCommand = "cd \"$HOME/rscoin\""

updateRepoCommand :: T.Text
updateRepoCommand =
    T.unlines ["git checkout .", "git checkout master -q", "git pull --ff-only"]

configYaml :: ShardParams -> T.Text
configYaml ShardParams{..} =
    T.unlines
        [ sformat ("shardDivider: " % int) spDivider
        , sformat ("shardDelta: " % int) spDelta]

setupConfigCommand :: ShardParams -> T.Text
setupConfigCommand =
    sformat ("echo '" % stext % "' > rscoin.yaml") . configYaml

profilingBuildArgs :: Bool -> T.Text
profilingBuildArgs True = " --profile --executable-profiling --library-profiling "
profilingBuildArgs False = ""

profilingRunArgs :: Bool -> T.Text
profilingRunArgs True = " +RTS -p -RTS "
profilingRunArgs False = ""

bankSetupCommand :: BankParams -> [T.Text] -> [C.PublicKey] -> T.Text
bankSetupCommand BankParams {..} mHosts mKeys =
    T.unlines
        [ cdCommand
        , bankStopCommand
        , "rm -rf bank-db"
        , updateRepoCommand
        , setupConfigCommand bpShardParams
        , mconcat ["stack build rscoin", profilingBuildArgs bpProfiling]
        , T.unlines $ map (uncurry addMintetteCommand) $ zip mHosts mKeys]
  where
    addMintetteCommand =
        sformat
            ("stack exec -- rscoin-bank add-mintette --port " % int %
             " --host " %
             stext %
             " --key " %
             build)
            (C.defaultPort :: Int)

bankRunCommand :: Word -> Bool -> T.Text
bankRunCommand periodDelta profiling =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-bank serve --log-severity Warning +RTS -qg -RTS --period-delta " %
               int %
               stext)
              periodDelta
              (profilingRunArgs profiling)]

bankStopCommand :: T.Text
bankStopCommand = "killall rscoin-bank"

mintetteKeyGenCommand :: ShardParams -> T.Text
mintetteKeyGenCommand shardParams =
    T.unlines
        [ cdCommand
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , updateRepoCommand
        , setupConfigCommand shardParams
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

usersCommand :: ShardParams -> T.Text -> Word -> Word -> T.Text
usersCommand shardParams bankHost u t =
    T.unlines
        [ cdCommand
        , updateRepoCommand
        , setupConfigCommand shardParams
        , sformat
              ("stack bench rscoin:rscoin-bench-only-users --benchmark-arguments \"--users " %
               int %
               " --bank " %
               build %
               " --transactions " %
               int %
               " +RTS -qg -RTS\"")
              u
              bankHost
              t]

sshArgs :: T.Text -> [T.Text]
sshArgs hostName =
    [ "-i"
    , sshKeyPath
    , "-o"
    , "StrictHostKeyChecking=no"
    , mconcat [userName, "@", hostName]]

runSsh :: T.Text -> T.Text -> IO ()
runSsh hostName command = do
    T.ExitSuccess <- T.proc "ssh" (sshArgs hostName ++ [command]) mempty
    return ()

runSshStrict :: T.Text -> T.Text -> IO T.Text
runSshStrict hostName command = do
    (T.ExitSuccess,res) <-
        T.procStrict "ssh" (sshArgs hostName ++ [command]) mempty
    return res

installRSCoin :: T.Text -> IO ()
installRSCoin = flip runSsh installCommand

runBank :: BankParams -> T.Text -> [T.Text] -> [C.PublicKey] -> IO ThreadId
runBank bp@BankParams{..} bankHost mintetteHosts mintetteKeys = do
    unless bpHasRSCoin $ installRSCoin bankHost
    runSsh bankHost $ bankSetupCommand bp mintetteHosts mintetteKeys
    forkIO $ runSsh bankHost $ bankRunCommand bpPeriodDelta bpProfiling

stopBank :: T.Text -> IO ()
stopBank bankHost = runSsh bankHost bankStopCommand

genMintetteKey :: ShardParams -> T.Text -> IO C.PublicKey
genMintetteKey sp hostName = do
    runSsh hostName $ mintetteKeyGenCommand sp
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: T.Text -> MintetteData -> IO ThreadId
runMintette bankHost (MintetteData hasRSCoin hostName) = do
    unless hasRSCoin $ installRSCoin hostName
    forkIO $ runSsh hostName $ mintetteRunCommand bankHost

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsers :: ShardParams -> T.Text -> UsersData -> Word -> Word -> IO ()
runUsers sp bankHost (UsersData hasRSCoin hostName) u t = do
    unless hasRSCoin $ installRSCoin hostName
    runSsh hostName $ usersCommand sp bankHost u t

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    RemoteConfig{..} <-
        readRemoteConfig $ fromMaybe "remote.yaml" $ rboConfigFile
    let sp = ShardParams rcShardDivider rcShardDelta
        bp =
            BankParams
            { bpPeriodDelta = rcPeriod
            , bpShardParams = sp
            , bpProfiling = bdProfiling rcBank
            , bpHasRSCoin = bdHasRSCoin rcBank
            }
        bankHost = bdHost rcBank
        mintettes = genericTake (fromMaybe maxBound rcMintettesNum) rcMintettes
    C.initLogging C.Error
    initBenchLogger $ fromMaybe C.Info $ rboBenchSeverity
    logInfo $ sformat ("Mintettes: " % build) $ listBuilderJSON mintettes
    mintetteKeys <- mapM (genMintetteKey sp . mdHost) mintettes
    mintetteThreads <- mapM (runMintette bankHost) mintettes
    logInfo "Launched mintettes, waiting…"
    T.sleep 2
    logInfo "Launching bank…"
    bankThread <- runBank bp bankHost (map mdHost mintettes) mintetteKeys
    logInfo "Launched bank, waiting…"
    T.sleep 3
    logInfo "Running users…"
    runUsers sp bankHost rcUsers rcUsersNum rcTransactionsNum
    logInfo "Ran users"
    killThread bankThread
    logInfo "Killed bank thread"
    stopBank bankHost
    logInfo "Stopped bank"
    mapM_ killThread mintetteThreads
    logInfo "Killed mintette threads"
    mapM_ stopMintette $ map mdHost mintettes
    logInfo "Stopped mintettes"
