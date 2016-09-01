module App.View.Alert where

import Prelude                        (const, ($), (==), (<<<))

import App.Types                      (State, Action (..))

import Data.Maybe                     (Maybe (..))

import Pux.Html                       (Html, div, text, span, button,
                                       strong)
import Pux.Html.Attributes            (aria, data_, type_, role)
import Pux.Html.Events                (onClick)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 (alert, alertDanger, alertDismissible,
                                       close)

view :: State -> Html Action
view state = div [] []
--    case state.error of
--        Just e ->
--            div
--                [ classNames [alert, alertDanger, alertDismissible]
--                , role "alert"
--                ]
--                [ button
--                    [ type_ "button"
--                    , className close
--                    , data_ "dismiss" "alert"
--                    , aria "label" "Close"
--                    , onClick $ const DismissError
--                    ]
--                    [ span
--                        [ aria "hidden" "true" ]
--                        [ text "×" ]
--                    ]
--                , strong
--                    []
--                    [ text "Error! " ]
--                , text e
--                ]
--        Nothing -> div [] []
