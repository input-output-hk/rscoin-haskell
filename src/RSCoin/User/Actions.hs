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
import           Control.Monad          (unless)
import           Control.Monad.Trans    (liftIO)
import           Data.Acid.Advanced     (query')
import           Data.Bifunctor         (bimap)
import           Data.Maybe             (fromJust, isJust)
import           Data.Monoid            ((<>))
import qualified Data.Text.IO           as TIO

import           Serokell.Util.Text     (format')

import           RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Error      (eWrap)
import           RSCoin.User.Operations (formTransactionRetry, getAmount,
                                         updateBlockchain)
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
processAction :: WorkMode m => A.RSCoinUserState -> UserAction -> m ()
processAction st ListAddresses =
    eWrap $
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM (\w -> (w ^. W.publicAddress, ) <$> getAmount st w) =<<
           query' st GetAllAddresses
       liftIO $ do
           TIO.putStrLn "Here's the list of your accounts:"
           TIO.putStrLn "# | Public ID                                    | Amount"
           mapM_ (TIO.putStrLn . format' "{}.  {} : {}") $
               uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
processAction st (FormTransaction inputs outputAddrStr) =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (bimap fromIntegral C.Coin) inputs
       unless (isJust pubKey) $
           P.commitError $
           "Provided key can't be exported: " <> outputAddrStr
       formTransactionRetry 2 st True inputs' (fromJust pubKey) $
           C.Coin (sum $ map snd inputs)
processAction st UpdateBlockchain =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo C.userLoggerName $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
