module App.ViewNew.Header where

import Prelude                        (const, ($), (==), (<<<), bind, (<>))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, noBorder)


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_, className, id_,
                                       placeholder, value, src, alt)

import Pux.CSS                        (style, backgroundColor, padding, px,
                                       color, white, backgroundImage, url)

view :: State -> Html Action
view state =
    nav
        [ className "navbar navbar-default"
        , style do
            backgroundImage $ url headerBitmapPath
            noBorder
        ]
        [ div
            [ className "container-fluid"
            , style do
                backgroundColor darkRed
                opacity 0.8
            ]
            [ div
                [ className "navbar-header" ]
                [ R.link R.homeUrl
                    [ className "navbar-brand"
                    , style do
                        color white
                        let p = px 10.0
                        padding p p p p
                    ]
                    [ img
                        [ alt "Brand"
                        , src logoPath
                        ]
                        []
                    ]
                ]
            , ul
                [ className "nav navbar-nav navbar-right" ]
                [ li
                    [ className "dropdown" ]
                    [ a
                        [ className "dropdown-toggle"
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        , style $ color white
                        ]
                        [ text "ADA"
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
                        , className "form-control"
                        , placeholder "Address / Transaction"
                        ] []
                    , span
                        [ className "input-group-btn" ]
                        [ button
                            [ className "btn btn-default"
                            ]
                            [ span
                                [ className "glyphicon glyphicon-search"
                                , aria "hidden" "true"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
