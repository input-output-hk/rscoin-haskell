{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.ErrorMessage (reportSimpleError) where

import           Control.Monad   (void)

import           Graphics.UI.Gtk (AttrOp ((:=)))
import qualified Graphics.UI.Gtk as G

reportSimpleError :: G.Window -> String -> IO ()
reportSimpleError mw message = do
    dialog <- G.messageDialogNew
        (Just mw)
        [G.DialogDestroyWithParent]
        G.MessageError
        G.ButtonsClose
        message
    G.set dialog [G.windowTitle := "RSCoin Error"]
    void $ G.dialogRun dialog
    G.widgetDestroy dialog
