module App.View.Header where

import Prelude                     (const, ($), (==), (<<<))

import App.Routes                  (homeUrl) as R
import App.Types                   (State, Action (..))


import Pux.Html                    (Html, div, text, span, button,
                                    input, a, li, ul, nav)
import Pux.Router                  (link) as R
import Pux.Html.Attributes         (className, aria, data_, type_,
                                    placeholder, value)
import Pux.Html.Events             (onChange, onClick, onKeyDown)

view :: State -> Html Action
view state =
    nav
        [ className "navbar navbar-default" ]
        [ div
            [ className "container-fluid" ]
            [ div
                [ className "navbar-header" ]
                [ R.link R.homeUrl
                    [ className "navbar-brand"
                    ]
                    [ text "RS | COIN" ]
                ]
            , ul
                [ className "nav navbar-nav navbar-right" ]
                [ li
                    [ className "dropdown" ]
                    [ a
                        [ className "dropwdown-toggle"
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        ]
                        [ text "English"
                        , span
                            [ className "caret" ]
                            []
                        ]
                    , ul
                        [ className "dropdown-menu" ]
                        [ li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "English" ]
                            ]
                        ]
                    ]
                ]
            , div
                [ className "col-xs-6 navbar-form navbar-right" ]
                [ div
                    [ className "input-group" ]
                    [ input
                        [ type_ "text"
                        , value state.searchQuery
                        , onChange $ SearchQueryChange <<< _.value <<< _.target
                        , onKeyDown $ \e -> if e.keyCode == 13 then SearchButton else Nop
                        , className "form-control"
                        , placeholder "Address / Transaction"
                        ] []
                    , span
                        [ className "input-group-btn" ]
                        [ button
                            [ onClick $ const SearchButton
                            , className "btn btn-danger"
                            ] [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]
