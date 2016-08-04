{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import           Control.Concurrent      (ThreadId, forkIO, killThread)
import           Control.Exception       (finally)
import           Control.Monad           (forM_)
import           Control.Monad.Catch     (bracket)
import           Control.Monad.Extra     (whenJust)
import           Control.Monad.Trans     (MonadIO (liftIO))
import qualified Data.Acid               as ACID
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (cs)
import           Formatting              (sformat, stext, string, (%))
import qualified Options.Applicative     as Opts
import           Serokell.Util.OptParse  (strArgument)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withTempDirectory)
import qualified Turtle                  as Cherepakha

import qualified RSCoin.Bank             as B
import qualified RSCoin.Core             as C
import qualified RSCoin.Explorer         as E
import qualified RSCoin.Mintette         as M
import qualified RSCoin.Notary           as N
import           RSCoin.Timed            (Second, fork_, runRealModeBank,
                                          runRealModeUntrusted)
import qualified RSCoin.User             as U

import           Config                  (BankData (..), DeployConfig (..),
                                          ExplorerData (..), MintetteData (..),
                                          NotaryData (..), readDeployConfig)

optionsParser :: Opts.Parser FilePath
optionsParser =
    strArgument $ mconcat [Opts.value "local.yaml", Opts.showDefault]

getConfigPath :: IO FilePath
getConfigPath =
    Opts.execParser $
    Opts.info
        (Opts.helper <*> optionsParser)
        (mconcat
             [ Opts.fullDesc
             , Opts.progDesc "Wrapper tool to deploy rscoin locally"])

data CommonParams = CommonParams
    { cpBaseDir :: FilePath
    , cpPeriod  :: Word
    } deriving (Show)

toModernFilePath :: FilePath -> Cherepakha.FilePath
toModernFilePath = Cherepakha.fromText . cs

startMintette :: FilePath
              -> CommonParams
              -> (Word, MintetteData)
              -> IO (ThreadId, C.PublicKey)
startMintette confPath CommonParams{..} (idx,_) = do
    let workingDir = cpBaseDir </> mconcat ["mintette-workspace-", show idx]
        workingDirModern = toModernFilePath workingDir
        port = mintettePort idx
        dbDir = workingDir </> "mintette-db"
    Cherepakha.mkdir workingDirModern
    (sk,pk) <- C.keyGen
    let start =
            runRealModeUntrusted (Just confPath) $
            bracket (liftIO $ M.openState dbDir) (liftIO . M.closeState) $
            \st ->
                 do fork_ $ M.runWorker sk st Nothing
                    M.serve port st sk
    (, pk) <$> forkIO start

startExplorer
    :: FilePath
    -> Maybe C.Severity
    -> CommonParams
    -> (Word, ExplorerData)
    -> IO (ThreadId, C.PublicKey)
startExplorer confPath severity CommonParams{..} (idx,ExplorerData{..}) = do
    let workingDir = cpBaseDir </> mconcat ["explorer-workspace-", show idx]
        workingDirModern = toModernFilePath workingDir
        portRpc = explorerPort idx
        portWeb = explorerWebPort idx
        dbDir = workingDir </> "explorer-db"
    Cherepakha.mkdir workingDirModern
    (sk,pk) <- C.keyGen
    let start =
            E.launchExplorerReal
                portRpc
                portWeb
                (fromMaybe C.Warning severity)
                dbDir
                (Just confPath)
                sk
    (, pk) <$> forkIO start

startNotary :: FilePath
            -> Maybe C.Severity
            -> CommonParams
            -> NotaryData
            -> IO ThreadId
startNotary confPath severity CommonParams{..} NotaryData{..} = do
    let workingDir = cpBaseDir </> "notary-workspace"
        workingDirModern = toModernFilePath workingDir
        dbDir = workingDir </> "notary-db"
        start =
            N.launchNotaryReal
                (fromMaybe C.Warning severity)
                (Just dbDir)
                (Just confPath)
                8090
    Cherepakha.mkdir workingDirModern
    forkIO start

type PortsAndKeys = [(Int, C.PublicKey)]

startBank
    :: FilePath
    -> CommonParams
    -> PortsAndKeys
    -> PortsAndKeys
    -> BankData
    -> IO ThreadId
startBank confPath CommonParams{..} mintettes explorers BankData{..} = do
    let workingDir = cpBaseDir </> "bank-workspace"
        workingDirModern = toModernFilePath workingDir
        dbDir = workingDir </> "bank-db"
        periodDelta :: Second = fromIntegral cpPeriod
    Cherepakha.mkdir workingDirModern
    bankSk <- C.readSecretKey bdSecret
    forM_
        explorers
        (\(port,key) ->
              B.addExplorerInPlace
                  (Just confPath)
                  bankSk
                  dbDir
                  (C.Explorer C.localhost port key)
                  0)
    forM_
        mintettes
        (\(port,key) ->
              B.addMintetteInPlace
                  (Just confPath)
                  bankSk
                  dbDir
                  (C.Mintette C.localhost port)
                  key)
    forkIO $ B.launchBankReal periodDelta dbDir (Just confPath) bankSk

-- TODO: we can setup other users similar way
setupBankUser :: FilePath -> CommonParams -> BankData -> IO ()
setupBankUser confPath CommonParams{..} BankData{..} = do
    let workingDir = cpBaseDir </> "user-workspace-bank"
        workingDirModern = toModernFilePath workingDir
        dbDir = workingDir </> "wallet-db"
        addressesNum = 5
        walletPathArg = sformat (" --wallet-path " % string % " ") dbDir
    Cherepakha.mkdir workingDirModern
    bankSk <- C.readSecretKey bdSecret
    runRealModeBank (Just confPath) bankSk $
        bracket
            (liftIO $ U.openState dbDir)
            (\st ->
                  liftIO $
                  do ACID.createCheckpoint st
                     U.closeState st) $
        \st ->
             U.initState st addressesNum $ Just bdSecret
    Cherepakha.echo $
        sformat ("Initialized bank user, db is stored in " % string) dbDir
    -- doesn't work, invent something better
    -- Cherepakha.export "BU_ARG" walletPathArg
    Cherepakha.echo
        "Use command below to do smth as bank user"
    Cherepakha.echo $
        sformat ("stack $NIX_STACK exec -- rscoin-user " % stext) walletPathArg

mintettePort :: Integral a => a -> Int
mintettePort = (C.defaultPort + 1 +) . fromIntegral

explorerPort :: Integral a => a -> Int
explorerPort = (C.defaultPort + 3000 +) . fromIntegral

explorerWebPort :: Integral a => a -> Int
explorerWebPort = (C.defaultPort + 5000 +) . fromIntegral

main :: IO ()
main = do
    confPath <- C.defaultConfigurationPath
    deployLocalYamlPath <- getConfigPath
    DeployConfig{..} <- readDeployConfig deployLocalYamlPath
    let makeAbsolute path =
            ((</> path) . cs . either (error . show) id . Cherepakha.toText) <$>
            Cherepakha.pwd
        maybeInitLogger mSev name =
            whenJust mSev $ flip C.initLoggerByName name
    absoluteDir <- makeAbsolute dcDirectory
    absoluteBankSecret <- makeAbsolute (bdSecret dcBank)
    C.initLogging dcGlobalSeverity
    maybeInitLogger dcBankSeverity C.bankLoggerName
    maybeInitLogger dcNotarySeverity C.notaryLoggerName
    maybeInitLogger dcMintetteSeverity C.mintetteLoggerName
    maybeInitLogger dcExplorerSeverity C.explorerLoggerName
    withTempDirectory absoluteDir "rscoin-deploy" $
        \tmpDir -> do
            let cp =
                    CommonParams
                    { cpBaseDir = tmpDir
                    , cpPeriod = dcPeriod
                    }
                bd =
                    dcBank
                    { bdSecret = absoluteBankSecret
                    }
                mintettePorts = map mintettePort [0 .. length dcMintettes - 1]
                explorerPorts = map explorerPort [0 .. length dcExplorers - 1]
            (mintetteThreads,mintetteKeys) <-
                unzip <$>
                mapM (startMintette confPath cp) (zip [0 ..] dcMintettes)
            (explorerThreads,explorerKeys) <-
                unzip <$>
                mapM
                    (startExplorer confPath dcExplorerSeverity cp)
                    (zip [0 ..] dcExplorers)
            let mintettes = zip mintettePorts mintetteKeys
                explorers = zip explorerPorts explorerKeys
            notaryThread <- startNotary confPath dcNotarySeverity cp dcNotary
            bankThread <- startBank confPath cp mintettes explorers bd
            setupBankUser confPath cp bd
            Cherepakha.echo "Deployed successfully!"
            Cherepakha.sleep 100500 `finally`
                (mapM_ killThread $
                 bankThread : notaryThread : mintetteThreads ++ explorerThreads)
