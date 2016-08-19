{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

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
import           RSCoin.Notary.Error     (NotaryError)
import qualified RSCoin.Notary.Server    as S

import           Servant                 ((:<|>) (..), (:>), (:~>) (Nat), Get,
                                          Handler, Header, Headers, JSON, Post,
                                          Proxy (Proxy), ReqBody, ServerT,
                                          StdMethod (OPTIONS), Verb, addHeader,
                                          enter, err500, serve)

type AllocateMSInput =
    ( C.Address
    , C.PartyAddress
    , C.AllocationStrategy
    , C.Signature
    , Maybe (C.PublicKey, C.Signature)
    )

type Options = Verb 'OPTIONS 200

type PreHeaders =
    Headers '[ Header "Access-Control-Allow-Origin" String
             , Header "Access-Control-Allow-Methods" String
             , Header "Access-Control-Allow-Headers" String]
            ()

type NotaryApi =
  "allocateMultisig" :>
      Options '[JSON] PreHeaders
  :<|>
  "allocateMultisig" :>
      ReqBody '[JSON] AllocateMSInput :>
      Post '[JSON] (Headers '[Header "Access-Control-Allow-Origin" String] ())
  :<|>
  "getPeriodId" :> Get '[JSON] C.PeriodId

notaryApi :: Proxy NotaryApi
notaryApi = Proxy

type MyHandler = ReaderT S.NotaryState IO

servantServer :: ServerT NotaryApi MyHandler
servantServer =
    return preHeaders
    :<|> (\arg -> do method S.handleAllocateMultisig arg
                     return $ addHeader "*" ())
    :<|> method0 S.handleGetPeriodId
  where
    preHeaders = addHeader "*" $
                 addHeader "POST, GET, OPTIONS" $
                 addHeader "X-PINGOTHER, Content-Type" ()
    method :: (Curry (t -> IO b) b1) =>
              (S.NotaryState -> b1) -> t -> MyHandler b
    method act arg = ask >>= \st -> liftIO (uncurryN (act st) arg)
    method0 act = ask >>= \st -> liftIO (act st)

convertHandler :: forall a . S.NotaryState -> MyHandler a -> Handler a
convertHandler st act = liftIO (runReaderT act st) `catch` handler
  where
    handler (e :: NotaryError) = do
        C.logError $ sformat build e
        -- @TODO improve errors (they should actually mean smth)
        throwError err500

servantApp :: S.NotaryState -> Application
servantApp st = serve notaryApi $ enter nat servantServer
  where
    nat :: MyHandler :~> Handler
    nat = Nat $ convertHandler st
