import Graphics.UI.Gtk

import Controller
import Wallet

main :: IO ()
main = do
    let layoutFile = "src/GUI/GUI.glade"

    _ <- initGUI

    initializeController layoutFile $ Wallet "MY ADDRESS" 0 0 [] []

    mainGUI
