module App.Common.Html where

import Prelude (($), (<>))

import Pux.Html (Html, Attribute, text, span, div)
import Pux.Html.Attributes (className)

import Data.I18N (languageCode, Language, getTranslation, Translation)

ttext :: forall a. Language -> (Translation -> String) -> Html a
ttext = ttextClass ""

ttextUpper :: forall a. Language -> (Translation -> String) -> Html a
ttextUpper = ttextClass "text-uppercase"

ttextClass :: forall a. String -> Language -> (Translation -> String) -> Html a
ttextClass _class lang label =
    span
        [ className $ _class <> " language-" <> languageCode lang ]
        [ text $ label $ getTranslation lang ]

visible :: forall a. (Array (Attribute a) -> Array (Html a) -> Html a) -> Boolean -> Html a -> Html a
visible _ true t = t
visible tag false _ = tag [] []
