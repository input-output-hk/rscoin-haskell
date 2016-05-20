module GUI.RSCoin.GUI (startGUI) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)

import           Graphics.UI.Gtk        (on)
import qualified Graphics.UI.Gtk        as G

import           GUI.RSCoin.Glade       (MainWindow (..), importGlade)

startGUI :: IO ()
startGUI = do
    G.initGUI
    MainWindow {..} <- importGlade
    void $ window `on` G.deleteEvent $ liftIO G.mainQuit >> return False
    G.widgetShowAll window
    G.mainGUI
