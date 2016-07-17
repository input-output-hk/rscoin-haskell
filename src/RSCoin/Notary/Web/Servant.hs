{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RSCoin.Notary.Web.Servant
        ( servantApp
        ) where

import           Control.Monad.Catch     (catch)
import           Control.Monad.Except    (throwError)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.Trans     (liftIO)

import           Data.Tuple.Curry        (Curry, uncurryN)

import           Formatting              (build, sformat)

import           Network.Wai             (Application)

import qualified RSCoin.Core             as C
import           RSCoin.Notary.AcidState as S
import           RSCoin.Notary.Error     (NotaryError, logError)
import qualified RSCoin.Notary.Server    as S

import           Servant                 ((:<|>) (..), (:>), (:~>) (Nat),
                                          Get,
                                          Handler, JSON, Post, Proxy (Proxy),
                                          ReqBody, ServerT, enter, err500,
                                          serve)
type AllocateMSInput =
    ( C.Address
    , C.AllocationStrategy
    , (C.Address, C.Signature)
    , [(C.Signature, C.PublicKey)]
    )

type NotaryApi =
  "allocateMultisig"
    :> ReqBody '[JSON] AllocateMSInput
    :> Post '[JSON] ()
  :<|>
  "getPeriodId"
    :> Get '[JSON] C.PeriodId

notaryApi :: Proxy NotaryApi
notaryApi = Proxy

type MyHandler = ReaderT S.RSCoinNotaryState IO

servantServer :: ServerT NotaryApi MyHandler
servantServer = method S.handleAllocateMultisig
              :<|> method0 S.handleGetPeriodId
  where
    method :: (Curry (t -> IO b) b1) => (S.RSCoinNotaryState -> b1) -> t -> MyHandler b
    method act arg = ask >>= \st -> liftIO (uncurryN (act st) arg)
    method0 act = ask >>= \st -> liftIO (act st)

convertHandler :: forall a . S.RSCoinNotaryState -> MyHandler a -> Handler a
convertHandler st act = liftIO (runReaderT act st) `catch` handler
  where
    handler (e :: NotaryError) = do
        logError $ sformat build e
        -- @TODO improve errors (they should actually mean smth)
        throwError err500

servantApp :: S.RSCoinNotaryState -> Application
servantApp st = serve notaryApi $ enter nat servantServer
  where
    nat :: MyHandler :~> Handler
    nat = Nat $ convertHandler st
