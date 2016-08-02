module App.Config where

import Prelude (map, (<>), pure, (==), bind, (||))

import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)

import DOM               (DOM)
import DOM.HTML          (window)
import DOM.HTML.Window   (location)
import DOM.HTML.Location (hostname) as L

hostname :: forall eff. Eff (dom :: DOM | eff) String
hostname = window >>= location >>= L.hostname

-- FIXME: this is a workaround for RSC-108: https://issues.serokell.io/issue/RSC-108
wsUrl :: forall eff. Eff (dom :: DOM | eff) String
wsUrl = do
    hn <- hostname
    if hn == "localhost" || hn == "127.0.0.1"
        then wsUrlDebug
        else wsUrlProduction

wsUrlProduction :: forall eff. Eff (dom :: DOM | eff) String
wsUrlProduction = map (\h -> "ws://" <> h <> "/websocket") hostname

wsUrlDebug :: forall eff. Eff (dom :: DOM | eff) String
wsUrlDebug = pure "ws://127.0.0.1:8000"
