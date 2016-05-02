import           Control.Lens         (view, (^.))
import           Control.Monad.Trans  (MonadIO, liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.Acid            (update)
import           Data.Default         (def)
import           System.Random        (mkStdGen)

import           RSCoin.Bank          as B
import           RSCoin.Mintette      as M
import           RSCoin.User          as U
import           RSCoin.Core          (initLogging, Severity(Info), Mintette(..))
import           RSCoin.Test          (WorkMode, runRealMode, runEmulationMode,
                                       upto, mcs, work, minute, wait, for, sec)
import           Context              (TestEnv, mkTestContext, state, port, 
                                       keys, publicKey, secretKey, MintetteInfo,
                                       bank, mintettes, lifetime,
                                       ensureBankSecretKeyDefined, users, buser,
                                       UserInfo, bankSkPath, bankPkPath)



main :: IO ()
main = return ()

launchPure :: Int -> Int -> IO ()
launchPure mNum uNum = runEmulationMode (mkStdGen 9452) def $ launch mNum uNum

launch :: WorkMode m => Int -> Int -> m ()
launch mNum uNum = do
    liftIO $ initLogging Info

    (mkTestContext mNum uNum (minute 3) >>= ) $ runReaderT $ do
        runBank
        _ <- mapM runMintette =<< view mintettes

        wait $ for 5 sec  -- ensure that bank & mintettes have initialized
 
        _ <- mapM addMintetteToBank =<< view mintettes
        initBUser
        _ <- mapM initUser =<< view users

        return ()

    

runBank :: WorkMode m => TestEnv m ()
runBank = do
    b <- view bank
    l <- view lifetime
    work (upto $ mcs l) $ B.runWorker (b ^. secretKey) (b ^. state)
    work (upto $ mcs l) $ B.serve (b ^. state)
    
runMintette :: WorkMode m => MintetteInfo -> TestEnv m ()
runMintette m = do
    l <- view lifetime
    work (upto $ mcs l) $ 
        M.serve <$> view port <*> view state <*> view secretKey $ m
    work (upto $ mcs l) $
        M.runWorker <$> view secretKey <*> view state $ m

addMintetteToBank :: MonadIO m => MintetteInfo -> TestEnv m ()
addMintetteToBank mintette = do
    let addedMint = Mintette "127.0.0.1" (mintette ^. port)
        mintPKey  = mintette ^. publicKey        
    bankSt <- view $ bank . state
    liftIO $ update bankSt $ B.AddMintette addedMint mintPKey
 
initBUser :: WorkMode m => TestEnv m ()   
initBUser = do
    st <- view $ buser . state
    skPath <- bankSkPath
    U.initState st 5 (Just skPath)

initUser :: WorkMode m => UserInfo -> TestEnv m ()
initUser user = U.initState (user ^. state) 5 Nothing
    


