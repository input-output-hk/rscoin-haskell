{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent         (ThreadId, forkIO, killThread)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception          (finally)
import           Control.Monad              (unless)
import           Data.FileEmbed             (embedStringFile,
                                             makeRelativeToProject)
import           Data.List                  (genericLength, genericTake)
import           Data.Maybe                 (fromMaybe, isJust)
import qualified Data.Text                  as T (unlines)
import qualified Data.Text.IO               as TIO
import           Formatting                 (build, int, sformat, shown, stext,
                                             (%))
import qualified Options.Generic            as OG
import qualified Turtle                     as T

import           Serokell.Util              (listBuilderJSON)

import qualified RSCoin.Core                as C

import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.Remote.Config (BankData (..), MintetteData (..),
                                             ProfilingType (..),
                                             RemoteConfig (..), UserData (..),
                                             UsersData (..), readRemoteConfig)

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
    , bpProfiling   :: !(Maybe ProfilingType)
    , bpHasRSCoin   :: !Bool
    , bpBranch      :: !T.Text
    } deriving (Show)

data UsersParamsSingle = UsersParamsSingle
    { upsUsersNumber        :: !Word
    , upsMintettesNumber    :: !Word
    , upsTransactionsNumber :: !Word
    , upsShardParams        :: !ShardParams
    , upsDumpStats          :: !Bool
    , upsConfigStr          :: !T.Text
    , upsSeverity           :: !C.Severity
    , upsBranch             :: !T.Text
    , upsHasRSCoin          :: !Bool
    , upsHostName           :: !T.Text
    , upsBankHostName       :: !T.Text
    , upsProfiling          :: !(Maybe ProfilingType)
    } deriving (Show)

userName :: T.IsString s => s
userName = "ubuntu"

defaultBranch :: T.IsString s => s
defaultBranch = "master"

statsDir :: T.IsString s => s
statsDir = "\"$HOME\"/rscoin-stats"

sshKeyPath :: T.IsString s => s
sshKeyPath = "~/.ssh/rscointest.pem"

installCommand :: T.IsString s => s
installCommand = $(makeRelativeToProject "bench/install.sh" >>= embedStringFile)

cdCommand :: T.Text
cdCommand = "cd \"$HOME/rscoin\""

updateRepoCommand :: T.Text -> T.Text
updateRepoCommand branchName =
    T.unlines
        [ cdCommand
        , "git checkout . -q"
        , "git fetch"
        , sformat
              ("git checkout -q -b " % stext % " " % stext %
               " 2> /dev/null || git checkout -q " %
               stext)
              branchName
              originBranchName
              branchName
        , sformat ("git pull -f -q origin " % stext) branchName]
  where
    originBranchName = sformat ("origin/" % stext) branchName

updateTimezoneCommand :: T.Text
updateTimezoneCommand = "sudo timedatectl set-timezone Europe/Moscow"

configYaml :: ShardParams -> T.Text
configYaml ShardParams{..} =
    T.unlines
        [ sformat ("shardDivider: " % int) spDivider
        , sformat ("shardDelta: " % int) spDelta]

setupConfigCommand :: ShardParams -> T.Text
setupConfigCommand =
    sformat ("echo '" % stext % "' > rscoin.yaml") . configYaml

profilingBuildArgs :: Maybe ProfilingType -> T.Text
profilingBuildArgs Nothing = ""
profilingBuildArgs (Just _) =
    " --profile --executable-profiling --library-profiling "

profilingRunArgs :: Maybe ProfilingType -> T.Text
profilingRunArgs Nothing = ""
profilingRunArgs (Just PTStandard) = " +RTS -p -RTS "
profilingRunArgs (Just PTDetailed) = " +RTS -P -RTS "
profilingRunArgs (Just PTMostDetailed) = " +RTS -pa -RTS "

bankSetupCommand :: BankParams -> [T.Text] -> [C.PublicKey] -> T.Text
bankSetupCommand BankParams {..} mHosts mKeys =
    T.unlines
        [ cdCommand
        , bankStopCommand
        , "rm -rf bank-db"
        , updateRepoCommand bpBranch
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

bankRunCommand :: Word -> Maybe ProfilingType -> T.Text
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
bankStopCommand = "killall -s SIGINT rscoin-bank 2> /dev/null"

mintetteSetupCommand :: T.Text -> ShardParams -> Maybe ProfilingType -> T.Text
mintetteSetupCommand branchName shardParams profiling =
    T.unlines
        [ cdCommand
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , updateRepoCommand branchName
        , setupConfigCommand shardParams
        , sformat
              ("stack build rscoin " % stext)
              (profilingBuildArgs profiling)
        , "stack exec -- rscoin-keygen"]

mintetteCatKeyCommand :: T.Text
mintetteCatKeyCommand = "cat \"$HOME\"/.rscoin/key.pub\n"

mintetteRunCommand :: T.Text -> Maybe ProfilingType -> T.Text
mintetteRunCommand bankHost profiling =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-mintette --log-severity Error +RTS -qg -RTS --bank-host " %
               stext %
               stext)
              bankHost
              (profilingRunArgs profiling)]

mintetteStopCommand :: T.Text
mintetteStopCommand = "killall -s SIGINT rscoin-mintette 2> /dev/null"

mkStatsDirCommand :: T.Text
mkStatsDirCommand = sformat ("mkdir -p " % stext) statsDir

statsTmpFileName :: T.IsString s => s
statsTmpFileName = "bench-tmp.txt"

csvStatsTmpFileName :: T.IsString s => s
csvStatsTmpFileName = "bench-tmp.csv"

csvStatsFileName :: (Monoid s, T.IsString s) => s
csvStatsFileName = mconcat [statsDir, "/", "stats.csv"]

usersCommandSingle :: UsersParamsSingle -> T.Text
usersCommandSingle UsersParamsSingle{..} =
    T.unlines
        ([ cdCommand
         , updateRepoCommand upsBranch
         , updateTimezoneCommand
         , setupConfigCommand upsShardParams
         , mkStatsDirCommand
         , sformat ("touch " % stext) csvStatsFileName
         , sformat
               ("stack bench rscoin:rscoin-bench-only-users " % stext %
                " --benchmark-arguments \"--users " %
                int %
                " --mintettes " %
                int %
                " --transactions " %
                int %
                " --severity " %
                shown %
                " --output " %
                stext %
                " --csv " %
                stext %
                " --csvPrefix " %
                stext %
                " --bank " %
                stext %
                stext %
                " +RTS -qg -RTS\"")
               (profilingBuildArgs upsProfiling)
               upsUsersNumber
               upsMintettesNumber
               upsTransactionsNumber
               upsSeverity
               statsTmpFileName
               csvStatsTmpFileName
               csvPrefix
               upsBankHostName
               (profilingRunArgs upsProfiling)] ++
         dealWithStats)
  where
    statsId = "`date +\"%m.%d-%H:%M:%S\"`"
    csvPrefix =
        sformat
            ("\\\"" % stext % ",`git show-ref --abbrev -s HEAD`,\\\"")
            statsId
    dealWithStats
      | upsDumpStats =
          [ sformat
                ("mv " % stext % " " % stext)
                statsTmpFileName
                (mconcat [statsDir, "/", statsId, ".stats"])
          , sformat
                ("echo `cat " % stext % "` >> " % stext)
                csvStatsTmpFileName
                csvStatsFileName
          , sformat ("rm -f " % stext) csvStatsTmpFileName
          , sformat
                ("echo '" % stext % "' > " % stext)
                upsConfigStr
                (mconcat [statsDir, "/", statsId, ".yaml"])]
      | otherwise =
          [ sformat
                ("rm -f " % stext % " " % stext)
                statsTmpFileName
                csvStatsTmpFileName]

sshArgs :: T.Text -> [T.Text]
sshArgs hostName =
    [ "-i"
    , sshKeyPath
    , "-o"
    , "StrictHostKeyChecking=no"
    , mconcat [userName, "@", hostName]]

runSsh :: T.Text -> T.Text -> IO ()
runSsh hostName command =
    () <$ T.proc "ssh" (sshArgs hostName ++ [command]) mempty

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

genMintetteKey :: T.Text -> ShardParams -> MintetteData -> IO C.PublicKey
genMintetteKey branchName sp (MintetteData _ hostName profiling) = do
    runSsh hostName $ mintetteSetupCommand branchName sp profiling
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: T.Text -> MintetteData -> IO ThreadId
runMintette bankHost (MintetteData hasRSCoin hostName profiling) = do
    unless hasRSCoin $ installRSCoin hostName
    forkIO $ runSsh hostName $ mintetteRunCommand bankHost profiling

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsersSingle :: UsersParamsSingle -> IO ()
runUsersSingle ups@UsersParamsSingle{..} = do
    unless upsHasRSCoin $ installRSCoin upsHostName
    runSsh upsHostName $ usersCommandSingle ups

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    let configPath = fromMaybe "remote.yaml" $ rboConfigFile
    RemoteConfig{..} <- readRemoteConfig configPath
    configStr <- TIO.readFile configPath
    let sp = ShardParams rcShardDivider rcShardDelta
        globalBranch = fromMaybe defaultBranch rcBranch
        bp =
            BankParams
            { bpPeriodDelta = rcPeriod
            , bpShardParams = sp
            , bpProfiling = bdProfiling rcBank
            , bpHasRSCoin = bdHasRSCoin rcBank
            , bpBranch = fromMaybe globalBranch $ bdBranch rcBank
            }
        bankHost = bdHost rcBank
        mintettes = genericTake (fromMaybe maxBound rcMintettesNum) rcMintettes
        userDatas Nothing = []
        userDatas (Just (UDSingle{..})) = [udsData]
        userDatas (Just (UDMultiple{..})) = udmUsers
        noStats =
            any
                isJust
                (bpProfiling bp :
                 (map udProfiling (userDatas rcUsers) ++
                  map mdProfiling mintettes))
    C.initLogging C.Error
    initBenchLogger $ fromMaybe C.Info $ rboBenchSeverity
    logInfo $
        sformat ("Setting up and launching mintettes " % build % "…") $
        listBuilderJSON mintettes
    mintetteKeys <- mapConcurrently (genMintetteKey globalBranch sp) mintettes
    mintetteThreads <- mapConcurrently (runMintette bankHost) mintettes
    logInfo "Launched mintettes, waiting…"
    T.sleep 2
    logInfo "Launching bank…"
    bankThread <- runBank bp bankHost (map mdHost mintettes) mintetteKeys
    logInfo "Launched bank, waiting…"
    T.sleep 3
    logInfo "Running users…"
    let runUsersAndFinish :: Maybe UsersData -> IO ()
        runUsersAndFinish Nothing = do
            logInfo
                "Running users was disabled in config. I have finished, RSCoin is ready to be used"
            logInfo
                "Have fun now. I am going to sleep, you can wish me good night."
            T.sleep 100500
        runUsersAndFinish (Just UDSingle{..}) =
            runUsersSingle $
            UsersParamsSingle
            { upsUsersNumber = udsNumber
            , upsMintettesNumber = genericLength mintettes
            , upsTransactionsNumber = udsTransactionsNum
            , upsShardParams = sp
            , upsDumpStats = not noStats
            , upsConfigStr = configStr
            , upsSeverity = fromMaybe C.Warning $ udSeverity udsData
            , upsBranch = fromMaybe globalBranch $ udBranch udsData
            , upsHasRSCoin = udHasRSCoin udsData
            , upsHostName = udHost udsData
            , upsBankHostName = bankHost
            , upsProfiling = udProfiling udsData
            }
        runUsersAndFinish (Just UDMultiple{..}) = undefined
        finishMintettesAndBank = do
            logInfo "Ran users"
            stopBank bankHost
            logInfo "Stopped bank"
            mapM_ stopMintette $ map mdHost mintettes
            logInfo "Stopped mintettes"
            killThread bankThread
            logInfo "Killed bank thread"
            mapM_ killThread mintetteThreads
            logInfo "Killed mintette threads"
    runUsersAndFinish rcUsers `finally` finishMintettesAndBank
