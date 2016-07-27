module App.View.Transaction where

import App.Types  (Action (..), State (..))
import App.RSCoin (Address)

import Pux.Html  (Html, (#), div, h2, text, bind)

view :: Address -> State -> Html Action
view addr state =
    div # do
        h2 # text "bla"
