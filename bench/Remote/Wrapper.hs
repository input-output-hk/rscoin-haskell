{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

import           Control.Concurrent         (ThreadId, forkIO, killThread)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception          (finally, onException)
import           Control.Monad              (unless)
import           Control.Monad.Extra        (unlessM)
import           Data.Char                  (isSpace)
import           Data.FileEmbed             (embedStringFile,
                                             makeRelativeToProject)
import           Data.List                  (genericLength, genericTake)
import           Data.Maybe                 (fromMaybe, isJust)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T (filter, lines, strip, unlines,
                                                  unpack)
import qualified Data.Text.Buildable        as B (build)
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO               as TIO
import           Data.Traversable           (for)
import           Formatting                 (build, float, int, sformat, shown,
                                             stext, (%))
import qualified Options.Generic            as OG
import           System.Directory           (doesFileExist)
import           System.IO.Temp             (withSystemTempDirectory)
import qualified Turtle                     as T

import           Serokell.Util.Text         (listBuilderCSV, listBuilderJSON)

import qualified RSCoin.Core                as C

import           Bench.RSCoin.Logging       (initBenchLogger, logInfo)
import           Bench.RSCoin.Remote.Config (BankData (..), MintetteData (..),
                                             ProfilingType (..),
                                             RemoteConfig (..), UserData (..),
                                             UsersData (..), readRemoteConfig)
import           Bench.RSCoin.UserCommons   (initializeBank, userThread)
import           Bench.RSCoin.UserSingle    (itemsAndTPS)

data RemoteBenchOptions = RemoteBenchOptions
    { rboConfigFile    :: Maybe FilePath
    , rboBenchSeverity :: Maybe C.Severity
    } deriving (Show, OG.Generic)

instance OG.ParseField  C.Severity
instance OG.ParseFields C.Severity
instance OG.ParseRecord C.Severity
instance OG.ParseRecord RemoteBenchOptions

rscoinConfigStr :: T.Text
rscoinConfigStr = C.rscoinConfigStr

data BankParams = BankParams
    { bpPeriodDelta :: !Word
    , bpProfiling   :: !(Maybe ProfilingType)
    , bpSeverity    :: !(Maybe C.Severity)
    , bpBranch      :: !T.Text
    } deriving (Show)

data UsersParamsSingle = UsersParamsSingle
    { upsUsersNumber        :: !Word
    , upsMintettesNumber    :: !Word
    , upsTransactionsNumber :: !Word
    , upsDumpStats          :: !Bool
    , upsConfigStr          :: !T.Text
    , upsSeverity           :: !C.Severity
    , upsBranch             :: !T.Text
    , upsHostName           :: !T.Text
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

keyGenCommand :: T.Text
keyGenCommand = "stack exec -- rscoin-keygen\n"

catKeyCommand :: T.Text
catKeyCommand = "cat \"$HOME\"/.rscoin/key.pub\n"

catAddressCommand :: T.Text -> T.Text
catAddressCommand bankHost =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-user dump-address 1 --bank-host " % stext)
              bankHost]

updateRepoCommand :: T.Text -> T.Text
updateRepoCommand branchName =
    T.unlines
        [ cdCommand
        , "git checkout . -q"
        , "git fetch -q"
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

setupConfigCommand :: T.Text
setupConfigCommand =
    sformat ("echo '" % stext % "' > rscoin.yaml") rscoinConfigStr

profilingBuildArgs :: Maybe ProfilingType -> T.Text
profilingBuildArgs Nothing = ""
profilingBuildArgs (Just _) =
    " --executable-profiling --library-profiling "

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
        , setupConfigCommand
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

bankRunCommand :: BankParams -> T.Text
bankRunCommand BankParams{..} =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-bank serve --log-severity " % shown %
               " +RTS -qg -RTS --period-delta " %
               int %
               stext % " | grep \"Finishing period took\"")
              (fromMaybe C.Warning bpSeverity)
              bpPeriodDelta
              (profilingRunArgs bpProfiling)]

bankStopCommand :: T.Text
bankStopCommand = "killall -s SIGINT rscoin-bank 2> /dev/null"

mintetteSetupCommand :: T.Text -> Maybe ProfilingType -> T.Text
mintetteSetupCommand branchName profiling =
    T.unlines
        [ cdCommand
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , updateRepoCommand branchName
        , setupConfigCommand
        , sformat
              ("stack build rscoin " % stext)
              (profilingBuildArgs profiling)
        , keyGenCommand]

mintetteRunCommand :: T.Text -> Maybe ProfilingType -> Maybe C.Severity-> T.Text
mintetteRunCommand bankHost profiling severity =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-mintette +RTS -qg -RTS --bank-host " %
               stext %
               " " %
               stext %
               " " %
               stext)
              bankHost
              (profilingRunArgs profiling)
              (maybe "" (sformat ("--log-severity " % shown)) severity)]

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

statsId :: (Monoid s, T.IsString s) => s
statsId = "`date +\"%m.%d-%H:%M:%S\"`"

usersCommandSingle :: UsersParamsSingle -> T.Text
usersCommandSingle UsersParamsSingle{..} =
    T.unlines
        ([ cdCommand
         , updateRepoCommand upsBranch
         , updateTimezoneCommand
         , setupConfigCommand
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
               (profilingRunArgs upsProfiling)] ++
         dealWithStats)
  where
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

userSetupCommand :: T.Text -> T.Text -> Maybe ProfilingType -> T.Text
userSetupCommand bankHost branchName profiling =
    T.unlines
        [ cdCommand
        , "rm -rf wallet-db"
        , updateRepoCommand branchName
        , setupConfigCommand
        , sformat
              ("stack bench rscoin --no-run-benchmarks " % stext)
              (profilingBuildArgs profiling)
        , sformat
              ("stack exec -- rscoin-user list --addresses-num 1 --bank-host " %
               stext % " > /dev/null")
              bankHost]

userUpdateCommand :: T.Text -> T.Text
userUpdateCommand bankHost =
    T.unlines
        [ cdCommand
        , sformat
              ("stack exec -- rscoin-user update --bank-host " % stext % " > /dev/null")
              bankHost]

userStatsFileName :: T.IsString s => s
userStatsFileName = "user-stats.txt"

userRunCommand :: Maybe Word -> T.Text -> Word -> UserData -> T.Text
userRunCommand logInterval bankHost txNum UserData{..} =
    T.unlines
        [ cdCommand
        , sformat
              ("stack bench  rscoin:rscoin-bench-single-user --benchmark-arguments \"" %
               stext %
               "\"")
              benchmarkArguments]
  where
    benchmarkArguments =
        sformat
            ("--walletDb wallet-db --bank " % stext % stext %
             " --transactions " %
             int %
             " --dumpStats " %
             stext %
             stext %
             stext %
             stext %
             " +RTS -qg -RTS")
            bankHost
            severityArg
            txNum
            userStatsFileName
            printDynamicArg
            logIntervalArg
            (profilingRunArgs udProfiling)
    severityArg     = maybe "" (sformat (" --severity " % shown % " ")) udSeverity
    printDynamicArg = maybe "" (const $ " --printDynamic") udPrintTPS
    logIntervalArg  =
        maybe "" (sformat (" --logInterval " % int % " ")) logInterval

userStopCommand :: T.Text
userStopCommand = "killall -s SIGINT rscoin-bench-single-user 2> /dev/null"

resultTPSCommand :: T.Text
resultTPSCommand =
    T.unlines
      [ cdCommand
      , "cat " <> userStatsFileName
      ]

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

installRSCoinChecked :: Maybe Bool -> T.Text -> IO ()
installRSCoinChecked hasRSCoin host =
    unless (fromMaybe True hasRSCoin) $ installRSCoin host

installRSCoinBank :: BankData -> IO ()
installRSCoinBank BankData{..} = installRSCoinChecked bdHasRSCoin bdHost

installRSCoinMintette :: MintetteData -> IO ()
installRSCoinMintette MintetteData{..} =
    installRSCoinChecked mdHasRSCoin mdHost

installRSCoinUser :: UserData -> IO ()
installRSCoinUser UserData{..} = installRSCoinChecked udHasRSCoin udHost

setupAndRunBank :: BankParams -> T.Text -> [T.Text] -> [C.PublicKey] -> IO ThreadId
setupAndRunBank bp@BankParams{..} bankHost mintetteHosts mintetteKeys = do
    runSsh bankHost $ bankSetupCommand bp mintetteHosts mintetteKeys
    forkIO $ runSsh bankHost $ bankRunCommand bp

stopBank :: T.Text -> IO ()
stopBank bankHost = runSsh bankHost bankStopCommand

genMintetteKey :: T.Text -> MintetteData -> IO C.PublicKey
genMintetteKey branchName (MintetteData _ hostName profiling _) = do
    runSsh hostName $ mintetteSetupCommand branchName profiling
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName catKeyCommand

runMintette :: T.Text -> MintetteData -> IO ThreadId
runMintette bankHost (MintetteData _ hostName profiling sev) =
    forkIO $ runSsh hostName $ mintetteRunCommand bankHost profiling sev

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsersSingle :: UsersParamsSingle -> IO ()
runUsersSingle ups@UsersParamsSingle{..} =
    runSsh upsHostName $ usersCommandSingle ups

genUserKey :: T.Text -> T.Text -> UserData -> IO C.PublicKey
genUserKey bankHost globalBranch UserData{..} = do
    runSsh udHost $
        userSetupCommand bankHost (fromMaybe globalBranch udBranch) udProfiling
    fromMaybe (error "FATAL: constructPulicKey failed") .
        C.constructPublicKey . T.filter (not . isSpace) <$>
        runSshStrict udHost (catAddressCommand bankHost)

sendInitialCoins :: Word -> [C.PublicKey] -> IO ()
sendInitialCoins txNum (map C.Address -> userAddresses) = do
    withSystemTempDirectory "tmp" impl
  where
    bankId = 0
    impl dir =
        userThread
            dir
            (const $ initializeBank txNum userAddresses)
            bankId

updateUser :: T.Text -> UserData -> IO ()
updateUser bankHost UserData {..} = runSsh udHost $ userUpdateCommand bankHost

runUser :: Maybe Word -> T.Text -> Word -> UserData -> IO ()
runUser logInteval bankHost txNum ud@UserData{..} =
    runSsh udHost $ userRunCommand logInteval bankHost txNum ud

stopUser :: T.Text -> IO ()
stopUser host = runSsh host userStopCommand

writeUserTPSInfo
    :: SingleRunParams
    -> Word
    -> [UserData]
    -> Double
    -> IO ()
writeUserTPSInfo SingleRunParams{..} txNum userDatas totalTPS = do
    homeStats <- (T.</> "rscoin-stats") <$> T.home
    T.mktree homeStats
    (T.ExitSuccess, dateUglyStr) <- T.procStrict "date" ["+\"%m.%d-%H:%M:%S\""] mempty

    let dateStr         = T.filter (/= '"') $ T.strip dateUglyStr
    let (Right stats)   = T.toText homeStats
    let dateFileName    = mconcat [stats, "/", dateStr, ".stats"]
    let mintettesNum    = length srpMintettes
    let usersNum        = length userDatas
    let dateStatsOutput = T.unlines
            [ sformat ("total TPS: "              % float) totalTPS
            , sformat ("dynamic TPS: "            % stext) "TODO"
            , sformat ("number of mintettes: "    % int)   mintettesNum
            , sformat ("number of users: "        % int)   usersNum
            , sformat ("number of transactions: " % int)   txNum
            , sformat ("period length: "          % int)   srpPeriodLength
            ]

    TIO.writeFile (T.unpack dateFileName) dateStatsOutput

    let statsFile = T.unpack $ mconcat [stats, "/", "stats.csv"]
    unlessM (doesFileExist statsFile) $ do
        let statsHeader = "time,TPS,usersNum,mintettesNum,txNum,period\n"
        TIO.writeFile statsFile statsHeader

    let builtCsv = listBuilderCSV
            [ B.build dateStr
            , B.build totalTPS
            , B.build usersNum
            , B.build mintettesNum
            , B.build txNum
            , B.build srpPeriodLength
            ]
    TIO.appendFile statsFile $ sformat (build % "\n") builtCsv

collectUserTPS :: SingleRunParams -> Word -> [UserData] -> IO T.Text
collectUserTPS srp txNum userDatas = do
  resultFiles  <- for userDatas $ \ud -> runSshStrict (udHost ud) resultTPSCommand
  let lastLines = map (last . T.lines) resultFiles
  let (_, _, totalTPS) = itemsAndTPS lastLines

  writeUserTPSInfo srp txNum userDatas totalTPS
  return $ sformat ("Total TPS: " % float) totalTPS

data SingleRunParams = SingleRunParams
    { srpConfigStr    :: T.Text
    , srpGlobalBranch :: Maybe T.Text
    , srpPeriodLength :: Word
    , srpUsers        :: Maybe UsersData
    , srpMintettes    :: [MintetteData]
    , srpBank         :: BankData
    , srpUsersNum     :: Word
    , srpTxNum        :: Word
    } deriving (Show)

remoteBench ::  SingleRunParams -> IO ()
remoteBench srp@SingleRunParams{..} = do
    let globalBranch = fromMaybe defaultBranch srpGlobalBranch
        bp =
            BankParams
            { bpPeriodDelta = srpPeriodLength
            , bpProfiling = bdProfiling srpBank
            , bpSeverity = bdSeverity srpBank
            , bpBranch = fromMaybe globalBranch $ bdBranch srpBank
            }
        !bankHost = error "Bank host is not defined!"
        mintettes = srpMintettes
        userDatas Nothing = []
        userDatas (Just (UDSingle{..})) = [udsData]
        userDatas (Just (UDMultiple{..})) = udmUsers
        noStats =
            any
                isJust
                (bpProfiling bp :
                 (map udProfiling (userDatas srpUsers) ++
                  map mdProfiling mintettes))
    logInfo $
        sformat ("Setting up and launching mintettes " % build % "…") $
        listBuilderJSON mintettes
    mintetteKeys <- mapConcurrently (genMintetteKey globalBranch) mintettes
    mintetteThreads <- mapConcurrently (runMintette bankHost) mintettes
    logInfo "Launched mintettes, waiting…"
    T.sleep 2
    logInfo "Launching bank…"
    bankThread <-
        setupAndRunBank bp bankHost (map mdHost mintettes) mintetteKeys
    logInfo "Launched bank, waiting…"
    T.sleep 3
    logInfo "Running users…"
    let runUsers :: Maybe UsersData -> IO ()
        runUsers Nothing = do
            logInfo
                "Running users was disabled in config. I have finished, RSCoin is ready to be used."
            logInfo
                "Have fun now. I am going to sleep, you can wish me good night."
            T.sleep 100500
        runUsers (Just UDSingle{..}) =
            runUsersSingle $
            UsersParamsSingle
            { upsUsersNumber = srpUsersNum
            , upsMintettesNumber = genericLength mintettes
            , upsTransactionsNumber = srpTxNum
            , upsDumpStats = not noStats
            , upsConfigStr = srpConfigStr
            , upsSeverity = fromMaybe C.Warning $ udSeverity udsData
            , upsBranch = fromMaybe globalBranch $ udBranch udsData
            , upsHostName = udHost udsData
            , upsProfiling = udProfiling udsData
            }
        runUsers (Just (UDMultiple{..})) = do
            let datas = genericTake srpUsersNum udmUsers
            runUsersMultiple datas srpTxNum udmLogInterval
        runUsersMultiple datas txNum logInterval =
            flip finally (TIO.putStrLn =<< collectUserTPS srp txNum datas) $
            do let stopUsers = () <$ mapConcurrently (stopUser . udHost) datas
               pks <- mapConcurrently (genUserKey bankHost globalBranch) datas
               sendInitialCoins txNum pks
               () <$ mapConcurrently (updateUser bankHost) datas
               () <$
                   mapConcurrently (runUser logInterval bankHost txNum) datas `onException`
                   stopUsers
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
    runUsers srpUsers `finally` finishMintettesAndBank

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    C.initLogging C.Error
    initBenchLogger $ fromMaybe C.Info $ rboBenchSeverity
    let configPath = fromMaybe "remote.yaml" $ rboConfigFile
    RemoteConfig{..} <- readRemoteConfig configPath
    configStr <- TIO.readFile configPath
    installRSCoinBank rcBank
    () <$ mapConcurrently installRSCoinMintette rcMintettes
    () <$ mapConcurrently installRSCoinUser (userDatas rcUsers)
    let paramsCtor periodDelta mintettesNum usersNum txNum =
            SingleRunParams
            { srpConfigStr = configStr
            , srpGlobalBranch = rcBranch
            , srpPeriodLength = periodDelta
            , srpUsers = rcUsers
            , srpBank = rcBank
            , srpMintettes = genericTake mintettesNum rcMintettes
            , srpUsersNum = usersNum
            , srpTxNum = txNum
            }
        mintettesNums = fromMaybe [maxBound] rcMintettesNum
    sequence_
        [remoteBench $ paramsCtor p mNum uNum txNum | p <- rcPeriod
                                                    , mNum <- mintettesNums
                                                    , uNum <- usersNums rcUsers
                                                    , txNum <- txNums rcUsers]
  where
    usersNums Nothing = [0]
    usersNums (Just (UDSingle{..})) = udsNumber
    usersNums (Just (UDMultiple{..})) =
        fromMaybe [genericLength udmUsers] udmNumber
    txNums Nothing = [0]
    txNums (Just (UDSingle{..})) = udsTransactionsNum
    txNums (Just (UDMultiple{..})) = udmTransactionsNum
    userDatas Nothing = []
    userDatas (Just UDSingle {..}) = [udsData]
    userDatas (Just UDMultiple {..}) = udmUsers
