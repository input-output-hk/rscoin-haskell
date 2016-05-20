module GUI.RSCoin.GUI (startGUI) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)

import           Graphics.UI.Gtk        (on)
import           Graphics.UI.Gtk        (AttrOp ((:=)))
import qualified Graphics.UI.Gtk        as G

import           GUI.RSCoin.Glade       (GladeMainWindow (..), importGlade)

data ModelNode = ModelNode
    { mStatus  :: String
    , mTime    :: String
    , mAddress :: String
    }

type Model = G.ListStore ModelNode

createRandomModel :: G.TreeView -> IO Model
createRandomModel view = do
    model <- G.listStoreNew []
    appendColumn model "Status" statusSetter
    appendColumn model "Time" timeSetter
    appendColumn model "Address" addrSetter
    G.treeViewSetModel view model
    return model
  where
    appendColumn model title attributesSetter = do
        column <- G.treeViewColumnNew
        G.treeViewColumnSetTitle column title
        renderer <- G.cellRendererTextNew
        G.cellLayoutPackStart column renderer False
        G.cellLayoutSetAttributes column renderer model attributesSetter
        void $ G.treeViewAppendColumn view column
    statusSetter ModelNode{..} = [G.cellText := mStatus]
    timeSetter ModelNode{..} = [G.cellText := mTime]
    addrSetter ModelNode{..} = [G.cellText := mAddress]

addRandomData :: Model -> IO ()
addRandomData model = mapM_ (G.listStoreAppend model) randomModelData
  where
    randomModelData = do
        st <- ["confirmed", "sent", "unknown"]
        tm <- ["4:19", "4:20", "12:00"]
        addr <- [ "A7FUZi67YbBonrD9TrfhX7wnnFxrIRflbMFOpI+r9dOc"
                , "G7FuzI67zbBbnrD9trfh27anNf2RiRFLBmfBPi+R9DBC"
                ]
        return $ ModelNode st tm addr

startGUI :: IO ()
startGUI = do
    void G.initGUI
    GladeMainWindow {..} <- importGlade
    model <- createRandomModel treeViewWallet
    addRandomData model
    void (window `on` G.deleteEvent $ liftIO G.mainQuit >> return False)
    G.widgetShowAll window
    G.mainGUI
