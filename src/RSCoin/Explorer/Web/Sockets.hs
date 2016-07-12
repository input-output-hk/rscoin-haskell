{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

-- | WebSockets part of Explorer Web Server.

module RSCoin.Explorer.Web.Sockets
       ( wsLoggerName
       , mkWsApp
       ) where

import           RSCoin.Explorer.Web.Sockets.App (mkWsApp, wsLoggerName)
