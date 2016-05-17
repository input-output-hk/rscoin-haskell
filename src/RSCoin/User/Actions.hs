{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module RSCoin.User.Actions
        ( UserAction (..)
        , processAction
        ) where

import           Data.Int               (Int64)
import           Data.Text              (Text)

import           Control.Lens           ((^.))
import           Control.Monad          (forM_, unless, when)
import           Control.Monad.Catch    (throwM)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Acid              as S
import           Data.Maybe             (fromJust, isJust)
import           Data.Monoid            ((<>))
import qualified Data.Text.IO           as TIO

import           Serokell.Util.Text     (format', formatSingle')

import           RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Error      (UserError (StorageError), eWrap)
import qualified RSCoin.User.Operations as P
import qualified RSCoin.User.Wallet     as W

-- | User command from core point of view (please use UserCommmand
data UserAction
    = ListAddresses                 -- ^ List all addresses in wallet,
                                    -- starting with 1
    | UpdateBlockchain              -- ^ Query bank to update wallet
                                    -- state according to blockchain
                                    -- status
    | FormTransaction [(Int, Int64)]
                      Text          -- ^ First argument represents
                                    -- inputs -- pairs (a,b), where a
                                    -- is index (starting from 1) of
                                    -- address in wallet, b is
                                    -- positive integer representing
                                    -- value to send.  Second argument
                                    -- represents the address to send,
                                    -- and amount
    deriving (Show)


-- | Given the state of program and command, makes correspondent
-- actions.
processAction
    :: WorkMode m => A.RSCoinUserState -> UserAction -> m ()
processAction st ListAddresses =
    liftIO $ eWrap $
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM (\w -> (w ^. W.publicAddress, ) <$> P.getAmount st w) =<<
           S.query st GetAllAddresses
       TIO.putStrLn "Here's the list of your accounts:"
       TIO.putStrLn "# | Public ID                                    | Amount"
       mapM_ (TIO.putStrLn . format' "{}.  {} : {}") $
           uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
processAction st (FormTransaction inputs outputAddrStr) =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
       unless (isJust pubKey) $
           P.commitError $
           "Provided key can't be exported: " <> outputAddrStr
       P.formTransaction st inputs (fromJust pubKey) $
           C.Coin (sum $ map snd inputs)
processAction st UpdateBlockchain =
    eWrap $
    do walletHeight <- liftIO $ S.query st A.GetLastBlockId
       liftIO $ TIO.putStrLn $
           formatSingle'
               "Current known blockchain's height (last HBLock's id) is {}."
               walletHeight
       lastBlockHeight <- pred <$> C.getBlockchainHeight
       when (walletHeight > lastBlockHeight) $
           throwM $
           StorageError $
           W.InternalError $
           format'
               ("Last block height in wallet ({}) is greater than last " <>
                "block's height in bank ({}). Critical error.")
               (walletHeight, lastBlockHeight)
       if lastBlockHeight == walletHeight
           then liftIO $ putStrLn "Blockchain is updated already."
           else do
               forM_
                  [walletHeight + 1 .. lastBlockHeight]
                  (\h -> do
                        liftIO $ TIO.putStr $
                            formatSingle' "Updating to height {} ..." h
                        P.updateToBlockHeight st h
                        liftIO $ TIO.putStrLn $
                            formatSingle' "updated to height {}" h)
               liftIO $ TIO.putStrLn "Successfully updated blockchain!"
