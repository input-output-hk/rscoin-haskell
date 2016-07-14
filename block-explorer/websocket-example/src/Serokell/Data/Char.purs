module Serokell.Data.Char
       ( isDigit
       , isSpace
       , isAlphanum
       , isLetter
       , isUpper
       , isLower
       , isPunctuation
       , module Char
       , module Re
       ) where

import Prelude

import Data.Char as Char
import Data.String as Str
import Data.String.Regex as Re

import Data.Either (fromRight)
import Partial.Unsafe (unsafePartial)

isDigit :: Char -> Boolean
isDigit c = Re.test (unsafePartial $ fromRight $ Re.regex "^\\d$" Re.noFlags) (Str.singleton c)

isSpace :: Char -> Boolean
isSpace c = Re.test (unsafePartial $ fromRight $ Re.regex "^\\s$" Re.noFlags) (Str.singleton c)

isAlphanum :: Char -> Boolean
isAlphanum c = Re.test (unsafePartial $ fromRight $ Re.regex "^\\w$" Re.noFlags) (Str.singleton c)

isLetter :: Char -> Boolean
isLetter c = Re.test (unsafePartial $ fromRight $ Re.regex "^[a-zA-Z]$" Re.noFlags) (Str.singleton c)

isPunctuation :: Char -> Boolean
isPunctuation c = Re.test (unsafePartial $ fromRight $ Re.regex "^[!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~]$" Re.noFlags) (Str.singleton c)

isUpper :: Char -> Boolean
isUpper c = isLetter c && c == Char.toUpper c

isLower :: Char -> Boolean
isLower c = isLetter c && c == Char.toLower c
