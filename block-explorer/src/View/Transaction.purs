module App.View.Transaction where

import App.Types  (Action (..), State (..))
import App.RSCoin (TransactionId)

import Pux.Html  (Html, (#), div, h2, text, bind)

view :: TransactionId -> State -> Html Action
view tId state =
    div # do
        h2 # text "bla"
