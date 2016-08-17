{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.ErrorMessage
       ( reportSimpleError
       , reportSimpleErrorNoWindow
       ) where

import           Control.Monad   (void)

import           Graphics.UI.Gtk (AttrOp ((:=)))
import qualified Graphics.UI.Gtk as G

reportSimpleError :: G.Window -> String -> IO ()
reportSimpleError mw = simpleError (Just mw)

reportSimpleErrorNoWindow :: String -> IO ()
reportSimpleErrorNoWindow = simpleError Nothing

simpleError :: Maybe G.Window -> String -> IO ()
simpleError maybeW message = do
    dialog <- G.messageDialogNew
        maybeW
        [G.DialogDestroyWithParent]
        G.MessageError
        G.ButtonsClose
        message
    G.set dialog [G.windowTitle := "RSCoin Error"]
    void $ G.dialogRun dialog
    G.widgetDestroy dialog
