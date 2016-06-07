{-# LANGUAGE TemplateHaskell #-}

import           Control.Concurrent (forkIO)
import           Control.Monad      (unless)
import           Data.FileEmbed     (embedStringFile, makeRelativeToProject)
import           Data.Functor       (void)
import           Data.Maybe         (fromMaybe)
import qualified Data.Text          as T (unlines)
import           Formatting         (build, int, sformat, (%))
import qualified Turtle             as T

import qualified RSCoin.Core        as C

userName :: T.IsString s => s
userName = "ubuntu"

sshKeyPath :: T.IsString s => s
sshKeyPath = "~/.ssh/rscointest.pem"

installCommand :: T.IsString s => s
installCommand = $(makeRelativeToProject "bench/install.sh" >>= embedStringFile)

bankCommand :: [T.Text] -> [C.PublicKey] -> T.Text
bankCommand mHosts mKeys =
    T.unlines
        [ "cd \"$HOME/rscoin\""
        , "killall rscoin-bank"
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
        , "killall rscoin-mintette"
        , "rm -rf mintette-db"
        , "stack exec -- rscoin-mintette --log-severity Error +RTS -qg-RTS"]

usersCommand :: Word -> T.Text
usersCommand n =
    T.unlines
        [ "cd \"$HOME/rscoin\""
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

runBank :: [T.Text] -> [C.PublicKey] -> Bool -> IO ()
runBank mintetteHosts mintetteKeys hasRSCoin =
    void . forkIO $
    do unless hasRSCoin $ installRSCoin C.bankHost
       runSsh C.bankHost $ bankCommand mintetteHosts mintetteKeys

genMintetteKey :: T.Text -> IO C.PublicKey
genMintetteKey hostName = do
    runSsh hostName mintetteKeyGenCommand
    fromMaybe (error "FATAL: constructPulicKey failed") . C.constructPublicKey <$>
        runSshStrict hostName mintetteCatKeyCommand

runMintette :: (Bool, T.Text) -> IO ()
runMintette (hasRSCoin, hostName) = void . forkIO $
   do unless hasRSCoin $ installRSCoin hostName
      runSsh hostName mintetteRunCommand

runUsers :: (Bool, T.Text) -> Word -> IO ()
runUsers (hasRSCoin,hostName) n = do
    unless hasRSCoin $ installRSCoin hostName
    runSsh hostName $ usersCommand n

main :: IO ()
main = do
    mintetteKeys <- mapM (genMintetteKey . snd) mintettes
    T.echo "Generated and deployed new mintette keys"
    mapM_ runMintette mintettes
    T.echo "Launched mintettes"
    T.sleep 2
    runBank (map snd mintettes) mintetteKeys True
    T.echo "Launched bank"
    T.sleep 5
    runUsers users 5
    T.echo "Launched users"
  where
    mintettes
        :: T.IsString s
        => [(Bool, s)]
    mintettes = [(True, "52.28.80.85")]
    users = (True, "52.28.239.209")
