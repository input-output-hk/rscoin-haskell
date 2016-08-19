module App.View.Header where

import Prelude                        (const, ($), (==), (<<<))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value)
import Pux.Html.Events                (onChange, onClick, onKeyDown)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

view :: State -> Html Action
view state =
    nav
        [ classNames [B.navbar, B.navbarDefault] ]
        [ div
            [ className B.containerFluid ]
            [ div
                [ className B.navbarHeader ]
                [ R.link R.homeUrl
                    [ className B.navbarBrand
                    ]
                    [ text "RS | COIN" ]
                ]
            , ul
                [ classNames [B.nav, B.navbarNav, B.navbarRight] ]
                [ li
                    [ className B.dropdown ]
                    [ a
                        [ className B.dropdownToggle
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        ]
                        [ text "English"
                        , span
                            [ className B.caret ]
                            []
                        ]
                    , ul
                        [ className B.dropdownMenu ]
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
                [ classNames [B.colXs6, B.navbarForm, B.navbarRight] ]
                [ div
                    [ className B.inputGroup ]
                    [ input
                        [ type_ "text"
                        , value state.searchQuery
                        , onChange $ SearchQueryChange <<< _.value <<< _.target
                        , onKeyDown $ \e -> if e.keyCode == 13 then SearchButton else Nop
                        , className B.formControl
                        , placeholder "Address / Transaction"
                        ] []
                    , span
                        [ className B.inputGroupBtn ]
                        [ button
                            [ onClick $ const SearchButton
                            , classNames [B.btn, B.btnDanger]
                            ] [ text "Search" ]
                        ]
                    ]
                ]
            ]
        ]
