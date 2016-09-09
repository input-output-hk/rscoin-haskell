{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module RSCoin.Notary.Web.Servant
        ( servantApp
        , AllocateMSInput (..)
        ) where

import           Control.Monad            (void)
import           Control.Monad.Catch      (catch)
import           Control.Monad.Except     (throwError)
import           Control.Monad.Reader     (ReaderT, ask, runReaderT)
import           Control.Monad.Trans      (liftIO)
import           Data.Tuple.Curry         (Curry, uncurryN)
import           Formatting               (build, sformat)
import           GHC.Generics             (Generic)
import           Network.Wai              (Application)
import           Servant                  ((:<|>) (..), (:>), (:~>) (Nat), Get,
                                           Handler, Header, Headers, JSON, Post,
                                           Proxy (Proxy), ReqBody, ServerT,
                                           StdMethod (OPTIONS), Verb, addHeader,
                                           enter, err500, serve)

import           Data.Aeson.TH            (deriveJSON)
import           Serokell.Util.Exceptions (throwText)

import qualified RSCoin.Core              as C
import           RSCoin.Core.AesonJS      ()
import           RSCoin.Notary.AcidState  as S
import           RSCoin.Notary.Error      (NotaryError)
import qualified RSCoin.Notary.Server     as S
import           Serokell.Aeson.Options   (defaultOptionsPS)


data AllocateMSInput = AllocateMSInput
    { msAddress      :: C.Address
    , partyAddress   :: C.PartyAddress
    , strategy       :: C.AllocationStrategy
    , signature      :: C.Signature (C.MSAddress, C.AllocationStrategy)
    , trustSignature :: Maybe (C.PublicKey, C.Signature C.PublicKey)
    }
  deriving (Show, Generic)

$(deriveJSON defaultOptionsPS ''AllocateMSInput)

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
    :<|> (\arg -> do void $ method S.handleAllocateMultisig $ amsInputToTuple arg
                     return $ addHeader "*" ())
    :<|> (method0 (\st -> fromRightWithFail =<< S.handleGetPeriodId st))
  where
    amsInputToTuple (AllocateMSInput {..}) = (msAddress, partyAddress, strategy, signature, trustSignature)
    fromRightWithFail (Left t)  = throwText t
    fromRightWithFail (Right a) = return a
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
