module App.View.BrowserInfo where

import Prelude (($))

import Pux.Html            (Html, (#), (##), (!), div, h2, text, a)
import Pux.Html.Attributes (className, href, target)

import App.Types           (State)
import App.Common.Html     (ttext)

view :: forall action. State -> Html action
view state =
    div # do
        h2 ! className "text-center" ##
            -- NOTE: monadic html is fixed in latest pux releast. Update!
            [ ttext state.language _._browserNotSupported
            , a
                [ href $ "https://www.google.com/chrome/browser/desktop/index.html"
                , target "_blank"
                ]
                [ text "chrome" ]
            , text ", "
            , a
                [ href $ "https://www.mozilla.org/en-US/firefox/new/"
                , target "_blank"
                ]
                [ text "firefox" ]
            ]
