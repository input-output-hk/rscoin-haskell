module App.ViewNew.NotFound where

import Pux.Html            (Html, (#), (##), (!), div, h2, text)
import Pux.Html.Attributes (className)

import App.Types           (State)
import App.Common.Html     (ttext)

view :: forall action. State -> Html action
view state =
    div # do
        h2 ! className "text-center" ##
            [ text "404 ", ttext state.language _.notFound ]
