import Control.Monad
import Control.Monad.IO.Class

import Graphics.UI.Gtk

main :: IO ()
main = do
    let layoutFile = "src/GUI/UserGUI.glade"
        tabsNames  = ["Wallet", "Contacts", "Send", "Transactions", "Options"]

    _ <- initGUI

    builder <- builderNew
    builderAddFromFile builder layoutFile

    window <- builderGetObject builder castToWindow "Window"
    _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

    buttons <- mapM (builderGetObject builder castToButton . flip (++) "Button") tabsNames
    tabs    <- mapM (builderGetObject builder castToBox) tabsNames

    forM_ (zip buttons tabs) $ \(button, tab) -> button `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShow tab

    widgetShowAll window
    mapM_ widgetHide tabs
    widgetShow $ head tabs

    mainGUI
