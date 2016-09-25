module App.View.Header where

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
import Pux.Html.Events                (onChange, onKeyDown)

import Data.I18N                      (getTranslation)

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
                        ]
                    ]
                ]
            , div
                [id_ "search-field-position"]
                [ input
                    [ type_ "search"
                    , id_ "search-field"
                    , placeholder $
                        translation.address <> " / " <> translation.transaction
                    , value state.searchQuery
                    , onChange $ SearchQueryChange <<< _.value <<< _.target
                    , onKeyDown $ \e -> if e.keyCode == 13 then SearchButton else Nop
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
  where
    translation = getTranslation state.language
