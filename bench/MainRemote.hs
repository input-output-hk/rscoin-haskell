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

import           Bench.RSCoin.RemoteConfig (RemoteConfig (..), readRemoteConfig)

data RemoteBenchOptions = RemoteBenchOptions
    { rboConfigFile :: Maybe FilePath
    } deriving (Show, OG.Generic)

instance OG.ParseRecord RemoteBenchOptions

userName :: T.IsString s => s
userName = "ubuntu"

sshKeyPath :: T.IsString s => s
sshKeyPath = "~/.ssh/rscointest.pem"

installCommand :: T.IsString s => s
installCommand = $(makeRelativeToProject "bench/install.sh" >>= embedStringFile)

bankRunCommand :: [T.Text] -> [C.PublicKey] -> T.Text
bankRunCommand mHosts mKeys =
    T.unlines
        [ "cd \"$HOME/rscoin\""
        , bankStopCommand
        , "rm -rf bank-db"
        , "git pull --ff-only"
        , "stack build rscoin"
        , mconcat $ map (uncurry addMintetteCommand) $ zip mHosts mKeys
        , "stack exec -- rscoin-bank serve --log-severity Warning +RTS -qg -RTS"]
  where
    addMintetteCommand =
        sformat
            ("stack exec -- rscoin-bank add-mintette --port " % int %
             " --host " %
             build %
             " --key " %
             build)
            (C.defaultPort :: Int)

bankStopCommand :: T.Text
bankStopCommand = "killall rscoin-bank"

mintetteKeyGenCommand :: T.Text
mintetteKeyGenCommand =
    T.unlines
        [ "cd \"$HOME/rscoin\""
        , "rm -rf mintette-db"
        , "git pull --ff-only"
        , "stack build rscoin"
        , "stack exec -- rscoin-keygen"]

mintetteCatKeyCommand :: T.Text
mintetteCatKeyCommand = "cat \"$HOME\"/.rscoin/key.pub\n"

mintetteRunCommand :: T.Text
mintetteRunCommand =
    T.unlines
        [ "cd \"$HOME/rscoin\""
        , mintetteStopCommand
        , "rm -rf mintette-db"
        , "stack exec -- rscoin-mintette --log-severity Error +RTS -qg-RTS"]

mintetteStopCommand :: T.Text
mintetteStopCommand = "killall rscoin-mintette"

usersCommand :: Word -> T.Text
usersCommand n =
    T.unlines
        [ "cd \"$HOME/rscoin\""
        , "git pull --ff-only"
        , sformat
              ("stack bench rscoin:rscoin-bench-only-users --benchmark-arguments \"--users " %
               int %
               " +RTS -qg\"")
              n]

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
    forkIO $ runSsh C.bankHost $ bankRunCommand mintetteHosts mintetteKeys

stopBank :: IO ()
stopBank = runSsh C.bankHost bankStopCommand

genMintetteKey :: T.Text -> IO C.PublicKey
genMintetteKey hostName = do
    runSsh hostName mintetteKeyGenCommand
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: (Bool, T.Text) -> IO ThreadId
runMintette (hasRSCoin,hostName) = do
    unless hasRSCoin $ installRSCoin hostName
    forkIO $ runSsh hostName mintetteRunCommand

stopMintette :: T.Text -> IO ()
stopMintette host = runSsh host mintetteStopCommand

runUsers :: (Bool, T.Text) -> Word -> IO ()
runUsers (hasRSCoin,hostName) n = do
    unless hasRSCoin $ installRSCoin hostName
    runSsh hostName $ usersCommand n

main :: IO ()
main = do
    RemoteBenchOptions{..} <- OG.getRecord "rscoin-bench-remote"
    RemoteConfig{..} <-
        readRemoteConfig $ fromMaybe "remote.yaml" $ rboConfigFile
    mintetteKeys <- mapM (genMintetteKey . snd) mintettes
    mintetteThreads <- mapM runMintette mintettes
    T.sleep 2
    bankThread <- runBank (map snd mintettes) mintetteKeys True
    T.sleep 5
    runUsers users usersNum
    killThread bankThread
    stopBank
    mapM_ killThread mintetteThreads
    mapM_ stopMintette $ map snd mintettes
  where
    mintettes
        :: T.IsString s
        => [(Bool, s)]
    mintettes = [(True, "52.28.80.85")]
    users = (True, "52.28.239.209")
