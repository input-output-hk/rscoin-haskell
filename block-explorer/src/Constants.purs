module App.Constants where

type Config =
    { secureWebSocket :: Boolean
    }

-- FIXME: this should probably be Eff (dom :: DOM | e) String , not just String
foreign import config :: Config

secureWebSocket :: Boolean
secureWebSocket = config.secureWebSocket
