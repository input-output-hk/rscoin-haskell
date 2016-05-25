{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.AddressesTab
    ( createAddressesTab
    , initAddressesTab
    ) where

import           Control.Lens          ((^.))
import           Control.Monad         (void)
import           Data.Acid             (update, query)

import           Graphics.UI.Gtk       (AttrOp ((:=)))
import qualified Graphics.UI.Gtk       as G

import qualified RSCoin.Core           as C
import           RSCoin.User           (RSCoinUserState)
import           RSCoin.User.AcidState (GetAllAddresses (..))
import           RSCoin.User.Wallet    (publicAddress)

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid    (GUIState)
import           GUI.RSCoin.MainWindow (MainWindow (..), AddressesTab (..))

createAddressesTab :: GladeMainWindow -> AddressesTab
createAddressesTab GladeMainWindow{..} = AddressesTab gTreeViewAddressesView

initAddressesTab :: RSCoinUserState -> GUIState -> MainWindow -> IO ()
initAddressesTab st gst MainWindow{..} = do
    let AddressesTab{..} = tabAddresses
    addresses <- query st GetAllAddresses
    addressesList <- G.listStoreNew addresses
    G.treeViewSetModel treeViewAddressesView addressesList
    addressesCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle addressesCol "Address"
    renderer <- G.cellRendererTextNew
    G.cellLayoutPackStart addressesCol renderer False
    G.cellLayoutSetAttributes addressesCol renderer addressesList $ \a ->
        [G.cellText := C.printPublicKey (a ^. publicAddress)]
    void $ G.treeViewAppendColumn treeViewAddressesView addressesCol
