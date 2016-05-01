import           RSCoin.Bank         as B
import           RSCoin.Mintette     as M
import           RSCoin.User         as U
import           RSCoin.Core
import           RSCoin.Test 

import           Control.Monad       (forM, replicateM)
import           Control.Monad.Trans (liftIO)
import           Data.Acid           (update)
import           System.Directory    (getDirectoryContents, removeDirectory)
import           System.Random       (randomIO)

main :: IO ()
main = return ()


launch :: WorkMode m => Int -> Int -> m ()
launch mNum uNum = do
    -- preparations
    liftIO $ do
        initLogging Info
        -- TODO: remove tmp/ 

    -- initializing acid states and keys
    bankSt <- liftIO $ B.openState =<< getClearState
    mintSt <- liftIO $ replicateM mNum $ M.openState =<< getClearState

    bankKeys <- liftIO keyGen
    mintKeys <- liftIO $ replicateM mNum $ keyGen

    -- run bank
    B.runWorker (secretKey bankKeys) bankSt
    B.serve bankSt    

    -- run mintettes
    forM [0 .. mNum - 1] $ \mid -> do
        M.serve 
            (makeMintettePort mid) 
            (mintSt !! mid) 
            (secretKey $ mintKeys !! mid)

    -- add mintettes to bank
    liftIO $ forM [0 .. mNum - 1] $ \mid -> do
        let mint = Mintette "127.0.0.1" (makeMintettePort mid)
        update bankSt $ B.AddMintette mint (publicKey $ mintKeys !! mid)
   

    return ()
  where
    secretKey = fst
    publicKey = snd
    

-- * info

getClearState :: IO FilePath
getClearState = do
--    existent <- getDirectoryContents statesDir
--    let files = [statesDir ++ "state" ++ show no | no <- [1..]]
--    return $ head $ dropWhile ( `elem` existent) files

    k <- randomIO :: IO Int
    return $ statesDir ++ "state" ++ show k

makeMintettePort :: Int -> Int
makeMintettePort = (+2300)

statesDir :: FilePath
statesDir = "tmp/"

keysDir :: FilePath
keysDir = "keys/"

mintetteKey :: Int -> String -> FilePath
mintetteKey n ext = mconcat 
    [ keysDir
    , "mintette"
    , show n
    , "/"
    , "mintette"
    , show n
    , "."
    , ext
    ]


