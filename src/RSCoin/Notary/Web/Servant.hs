{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module RSCoin.Notary.Web.Servant
       ( AllocateMSInput (..)
       , servantApp
       ) where

import           Control.Lens               (makeLenses, view)
import           Control.Monad.Catch        (MonadThrow, catch)
import           Control.Monad.Except       (throwError)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.Trans        (liftIO)

import           Data.Aeson.TH              (deriveJSON)
import           Data.Text                  (Text)
import           Formatting                 (build, sformat)
import           GHC.Generics               (Generic)
import           Network.MessagePack.Server (runServerT)
import           Network.Wai                (Application)
import           Servant                    ((:<|>) (..), (:>), (:~>) (Nat),
                                             Get, Handler, Header, Headers,
                                             JSON, Post, Proxy (Proxy), ReqBody,
                                             ServerT, StdMethod (OPTIONS), Verb,
                                             addHeader, enter, err500, serve)

import           Serokell.Util.Exceptions   (throwText)

import qualified RSCoin.Core                as C
import           RSCoin.Core.AesonJS        ()
import           RSCoin.Notary.AcidState    as S
import           RSCoin.Notary.Error        (NotaryError)
import qualified RSCoin.Notary.Server       as S
import           Serokell.Aeson.Options     (defaultOptionsPS)


data AllocateMSInput = AllocateMSInput
    { msAddress      :: C.Address
    , partyAddress   :: C.PartyAddress
    , strategy       :: C.AllocationStrategy
    , signature      :: C.Signature (C.MSAddress, C.AllocationStrategy)
    , trustSignature :: Maybe (C.PublicKey, C.Signature C.PublicKey)
    } deriving (Show, Generic)

deriveJSON defaultOptionsPS ''AllocateMSInput

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

data NotaryServantContext = NotaryServantContext
    { _notaryState :: S.NotaryState
    , _nodeContext :: C.NodeContext
    }

makeLenses ''NotaryServantContext

type NotaryServantHandler = ReaderT NotaryServantContext IO

servantServer :: ServerT NotaryApi NotaryServantHandler
servantServer =
    return preHeaders
    :<|>
        (\msAllocArgs -> do
            () <$ serveHandleAllocateMultisig msAllocArgs
            return $ addHeader "*" ()
        )
    :<|>
        serveHandleGetPeriodIdUnsigned
  where
    preHeaders = addHeader "*" $
                 addHeader "POST, GET, OPTIONS" $
                 addHeader "X-PINGOTHER, Content-Type" ()

    fromRightWithFail :: MonadThrow m => Either Text a -> m a
    fromRightWithFail = either throwText return

    serveHandleAllocateMultisig :: AllocateMSInput -> NotaryServantHandler ()
    serveHandleAllocateMultisig AllocateMSInput{..} = do
        st  <- view notaryState
        ctx <- view nodeContext
        res <- C.runRealModeWithContext ctx $ runServerT $ S.handleAllocateMultisig
            st
            msAddress
            partyAddress
            strategy
            signature
            trustSignature
        fromRightWithFail res

    serveHandleGetPeriodIdUnsigned :: NotaryServantHandler C.PeriodId
    serveHandleGetPeriodIdUnsigned = do
        st  <- view notaryState
        ctx <- view nodeContext
        res <- C.runRealModeWithContext ctx $ runServerT $ S.handleGetPeriodIdUnsigned st
        fromRightWithFail res

convertHandler :: S.NotaryState -> C.NodeContext -> NotaryServantHandler a -> Handler a
convertHandler st nodeCtx act = liftIO (runReaderT act servantContext) `catch` handler
  where
    servantContext = NotaryServantContext st nodeCtx
    handler (e :: NotaryError) = do
        C.logError $ sformat build e
        -- @TODO improve errors (they should actually mean smth)
        throwError err500

servantApp :: S.NotaryState -> C.NodeContext -> Application
servantApp st nodeCtx = serve notaryApi $ enter nat servantServer
  where
    nat :: NotaryServantHandler :~> Handler
    nat = Nat $ convertHandler st nodeCtx
