import Graphics.UI.Gtk

import Controller

main :: IO ()
main = do
    let layoutFile = "src/GUI/GUI.glade"

    _ <- initGUI

    initializeController layoutFile ProgrammState

    mainGUI
