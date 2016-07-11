-- | Web part of Explorer.

module RSCoin.Explorer.Web
       ( mkApplication
       ) where

import           Control.Monad.Trans            (MonadIO)
import           Network.Wai                    (Application)
import           Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets             as WS

import           RSCoin.Explorer.AcidState      (State)
import           RSCoin.Explorer.Channel        (Channel)
import           RSCoin.Explorer.Web.Servant    (servantApp)
import           RSCoin.Explorer.Web.Sockets    (mkWsApp)

mkApplication
    :: MonadIO m
    => Channel -> State -> m Application
mkApplication channel st = do
    wsApp <- mkWsApp channel st
    return $ websocketsOr WS.defaultConnectionOptions wsApp (servantApp st)
