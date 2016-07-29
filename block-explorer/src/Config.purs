module App.Config where

import Prelude (map, (<>), pure)

import Control.Bind ((>>=))
import Control.Monad.Eff (Eff)

import DOM               (DOM)
import DOM.HTML          (window)
import DOM.HTML.Window   (location)
import DOM.HTML.Location (hostname) as L

hostname :: forall eff. Eff (dom :: DOM | eff) String
hostname = window >>= location >>= L.hostname

wsUrl :: forall eff. Eff (dom :: DOM | eff) String
wsUrl = map (\h -> "ws://" <> h <> "/websocket") hostname

wsUrlDebug :: forall eff. Eff (dom :: DOM | eff) String
wsUrlDebug = pure "ws://localhost:8000"
