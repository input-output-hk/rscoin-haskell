{-# LANGUAGE OverloadedStrings #-}

module Error (handled) where

import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Exception              (SomeException (..))
import Control.Monad.Catch            (catch)
import Control.Monad.IO.Class         (liftIO)
import Control.Monad.STM              (atomically)
import Data.Text                      (isPrefixOf)

import Graphics.UI.Gtk                (labelSetText, postGUIAsync, widgetShowAll)

import Action                         (Action (..))
import OutputWidgets                  (OutputWidgets (..))
import RSCoin.Test                    (WorkMode)
import RSCoin.User.Error              (UserError (..))

handled ::
    WorkMode m => TBQueue Action -> Action -> OutputWidgets -> m () -> m ()
handled queue action ow job = catch (catch job userErrorHandler) handler
  where
    userErrorHandler :: WorkMode m => UserError -> m ()
    userErrorHandler e@(InputProcessingError t)
        | "Wallet isn't updated" `isPrefixOf` t = liftIO $ do
            atomically $ writeTBQueue queue Update
            atomically $ writeTBQueue queue action
        | otherwise = showError e
    userErrorHandler e = showError e

    handler :: WorkMode m => SomeException -> m ()
    handler (SomeException e) = showError e

    showError :: (Show e, WorkMode m) => e -> m ()
    showError e = liftIO $ postGUIAsync $ do
        labelSetText (messageLabel ow) $ show e
        widgetShowAll (notificationWindow ow)
