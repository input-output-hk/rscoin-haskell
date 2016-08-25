module App.ViewNew.Header where

import Prelude                        (const, ($), (==), (<<<), bind, (<>))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, noBorder,
                                       headFootHeight)


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
        [ className "navbar navbar-default navbar-fixed-top"
        , id_ "header-image" ]
        [ div
            [ className "container-fluid"
            , id_ "header-container"
            ]
            [ div
                [ className "navbar-header"
                , id_ "logo-link" ]
                [ R.link R.homeUrl
                    []
                    [ img
                        [ id_ "logo"
                        , src logoPath
                        ]
                        []
                    ]
                ]
            , ul
                [ className "nav navbar-nav navbar-right"
                , id_ "currency"]
                [ li
                    [ className "dropdown" ]
                    [ a
                        [ className "dropdown-toggle"
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        , id_ "toggle"
                        ]
                        [ text "ADA"
                        , span
                            [ className "caret"
                            , id_ "caret"]
                            []
                        ]
                    , ul
                        [ className "dropdown-menu"
                        , id_ "dropdown" ]
                        [ li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "ADA" ]
                            ]
                        , li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "YEN" ]
                            ]
                        , li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "USD" ]
                            ]
                        , li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "EUR" ]
                            ]
                        , li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "RUR" ]
                            ]
                        ]
                    ]
                ]
            , div
                [id_ "search-field-position"]
                [ input
                    [ type_ "search"
                    , id_ "search-field"
                    , placeholder "Address / IP / SHA hash"
                    ]
                    []
                , span
                    [ className "glyphicon glyphicon-search"
                    , id_ "search-ic"
                    ]
                    []
                ]
            ]
        ]
