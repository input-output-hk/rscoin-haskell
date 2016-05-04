import Graphics.UI.Gtk

import Control.Concurrent
import Control.Concurrent.STM.TBQueue

import Control.Monad.STM

import AcidExecutor
import Controller
import Updater

main :: IO ()
main = do
    let layoutFile = "src/GUI/GUI.glade"

    _ <- initGUI

    (ow, queue) <- initializeController layoutFile

    _ <- forkIO $ run queue ow
    _ <- forkIO $ runUpdater queue

    mainGUI
