module App.Config where

import Prelude (map, (<>), pure, (==), bind, (||), ($), (/=), const)

import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)

import DOM               (DOM)
import DOM.HTML          (window)
import DOM.HTML.Window   (location)
import DOM.HTML.Location (hostname) as L

import Data.Maybe        (Maybe (Nothing))
import Data.String       (stripPrefix)

import App.Constants (secureWebSocket)

hostname :: forall eff. Eff (dom :: DOM | eff) String
hostname = window >>= location >>= L.hostname

-- FIXME: this is a workaround for RSC-108: https://issues.serokell.io/issue/RSC-108
wsUrl :: forall eff. Eff (dom :: DOM | eff) String
wsUrl = localhostProduction (const wsUrlDebug) $ const wsUrlProduction

isAdmin :: forall eff. Eff (dom :: DOM | eff) Boolean
isAdmin = localhostProduction (const $ pure true) $ \hn -> pure $ stripPrefix "admin." hn /= Nothing

localhostProduction :: forall eff a. (String -> Eff (dom :: DOM | eff) a) -> (String -> Eff (dom :: DOM | eff) a) -> Eff (dom :: DOM | eff) a
localhostProduction localhost production = do
    hn <- hostname
    if hn == "localhost" || hn == "127.0.0.1"
        then localhost hn
        else production hn

protocol :: String
protocol = if secureWebSocket
             then "wss"
             else "ws"

wsUrlProduction :: forall eff. Eff (dom :: DOM | eff) String
wsUrlProduction = map (\h -> protocol <> "://" <> h <> "/websocket") hostname

wsUrlDebug :: forall eff. Eff (dom :: DOM | eff) String
wsUrlDebug = pure "ws://localhost:8001"
