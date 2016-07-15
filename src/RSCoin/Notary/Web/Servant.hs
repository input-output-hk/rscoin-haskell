{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module RSCoin.Notary.Web.Servant
        ( servantApp
        ) where

import           Control.Exception       (throwIO)
import           Control.Monad.Catch     (catch)
import           Control.Monad.IO.Class  (MonadIO, liftIO)

import           Data.Acid               (createCheckpoint)
import           Data.Acid.Advanced      (query', update')
import           Data.Text               (Text)

import           Formatting              (build, int, sformat, shown, (%))

import           Control.Monad.Catch     (Exception, catch, throwM)
import           Control.Monad.Except    (throwError)
import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.Trans     (liftIO)
import           Data.Acid.Advanced      (query')
import           Data.Tuple.Curry        (Curry, uncurryN)
import           Data.Typeable           (Typeable)
import           Network.Wai             (Application)
import qualified RSCoin.Core             as C
import qualified RSCoin.Core.Protocol    as P
import           RSCoin.Notary.AcidState as S
import           RSCoin.Notary.Error     (NotaryError)
import qualified RSCoin.Notary.Server    as S
import           RSCoin.Timed            (WorkMode (..), runRealMode)
import           Servant                 ((:<|>) (..), (:>), (:~>) (Nat),
                                          Capture,
                                          FromHttpApiData (parseUrlPiece), Get,
                                          Handler, JSON, Post, Proxy (Proxy),
                                          ReqBody, ServerT, enter, err500,
                                          parseUrlPiece, serve)

type PublishTxInput =
    ( C.Transaction
    , C.Address
    , (C.Address, C.Signature)
    )
type AllocateMSInput =
    ( C.Address
    , C.AllocationStrategy
    , (C.Address, C.Signature)
    , [(C.Signature, C.PublicKey)]
    )

type NotaryApi =
  "publishTx"
    :> ReqBody '[JSON] PublishTxInput
    :> Post '[JSON] [(C.Address, C.Signature)]
  :<|>
  "allocateMutisig"
    :> ReqBody '[JSON] AllocateMSInput
    :> Get '[JSON] ()
  :<|>
  "getPeriodId"
    :> Get '[JSON] C.PeriodId

notaryApi :: Proxy NotaryApi
notaryApi = Proxy

type MyHandler = ReaderT S.RSCoinNotaryState IO

servantServer :: ServerT NotaryApi MyHandler
servantServer = method S.handlePublishTx
              :<|> method S.handleAllocateMultisig
              :<|> method0 S.handleGetPeriodId
  where
    method :: (Curry (t -> IO b) b1) => (S.RSCoinNotaryState -> b1) -> t -> MyHandler b
    method act arg = ask >>= \st -> liftIO (uncurryN (act st) arg)
    method0 act = ask >>= \st -> liftIO (act st)

logError = C.logError C.notaryLoggerName

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
