module App.Common.Hash where

import Prelude ((<<<))

import Data.Function.Uncurried (Fn4, runFn4)


type DataB64U = String
type DataB64  = String

-- TODO: new Data.String 2.1.0 has replaceAll implemented
--
foreign import replaceR_ :: Fn4 String String String String String

-- Replace with regexp
replaceR ::
            String -> -- regexp
            String -> -- regexp modifiers
            String -> -- replacement
            String -> -- original string
            String
replaceR = runFn4 replaceR_

replaceAll reg = replaceR reg "g"

unescapeB64U :: DataB64U -> DataB64
unescapeB64U = replaceAll "-" "+" <<< replaceAll "_" "/"
