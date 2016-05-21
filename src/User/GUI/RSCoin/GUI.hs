module GUI.RSCoin.GUI (startGUI) where

import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)

import           Graphics.UI.Gtk        (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk        as G

import           GUI.RSCoin.Glade       (GladeMainWindow (..), importGlade)

data ModelNode = ModelNode
    { mStatus  :: String
    , mTime    :: String
    , mAddress :: String
    , mAmount  :: Integer
    }

type Model = G.ListStore ModelNode

createRandomModel :: G.TreeView -> IO Model
createRandomModel view = do
    model <- G.listStoreNew []
    appendColumn model True "Status" statusSetter
    appendColumn model True "Time" timeSetter
    appendColumn model True "Address" addrSetter
    appendColumn model True "Amount" amountSetter
    G.treeViewSetModel view model
    return model
  where
    appendColumn model expand title attributesSetter = do
        column <- G.treeViewColumnNew
        G.treeViewColumnSetTitle column title
        G.treeViewColumnSetExpand column expand
        renderer <- G.cellRendererTextNew
        G.cellLayoutPackStart column renderer False
        G.cellLayoutSetAttributes column renderer model attributesSetter
        void $ G.treeViewAppendColumn view column
    statusSetter ModelNode{..} = [G.cellText := mStatus]
    timeSetter ModelNode{..} = [G.cellText := mTime]
    addrSetter ModelNode{..} = [G.cellText := mAddress]
    amountSetter ModelNode{..} = [G.cellText := showSigned mAmount]
    showSigned a | a > 0     = "+" ++ show a
                 | otherwise = show a

addRandomData :: Model -> IO ()
addRandomData model = mapM_ (G.listStoreAppend model) randomModelData
  where
    randomModelData = do
        tm <- ["4:19", "4:20", "12:00"]
        am <- [123, -3456, 12345, -45323]
        st <- ["confirmed", "sent", "unknown"]
        addr <- [ "A7FUZi67YbBonrD9TrfhX7wnnFxrIRflbMFOpI+r9dOc"
                , "G7FuzI67zbBbnrD9trfh27anNf2RiRFLBmfBPi+R9DBC"
                ]
        return $ ModelNode st tm addr am

startGUI :: IO ()
startGUI = do
    void G.initGUI
    GladeMainWindow {..} <- importGlade
    model <- createRandomModel treeViewWallet
    addRandomData model
    void (window `on` G.deleteEvent $ liftIO G.mainQuit >> return False)
    G.widgetShowAll window
    G.mainGUI
