module GUI.RSCoin.AddressesTab
    ( createAddressesTab
    , initAddressesTab
    , updateAddressTab
    ) where

import           Control.Monad         (forM_, void, when)
import           Data.Acid             (update)

import           Graphics.UI.Gtk       (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk       as G

import           Serokell.Util.Text    (formatSingle')

import           GUI.RSCoin.Addresses  (VerboseAddress (..), getAddresses)
import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.MainWindow (AddressesTab (..), MainWindow (..))

import qualified RSCoin.Core           as C
import           RSCoin.User           (AddAddress (..), RSCoinUserState)

createAddressesTab :: GladeMainWindow -> IO AddressesTab
createAddressesTab GladeMainWindow{..} =
    AddressesTab
        gButtonGenerateAddress
        gButtonCopyAddress
        gTreeViewAddressesView
    <$> G.listStoreNew []

initAddressesTab :: RSCoinUserState -> MainWindow -> IO ()
initAddressesTab st mw@MainWindow{..} = do
    let AddressesTab{..} = tabAddresses
    G.treeViewSetModel treeViewAddressesView addressesModel
    addressesCol <- G.treeViewColumnNew
    balanceCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle addressesCol ("Address" :: String)
    G.treeViewColumnSetTitle balanceCol ("Balance" :: String)
    renderer <- G.cellRendererTextNew
    G.cellLayoutPackStart addressesCol renderer False
    G.cellLayoutPackStart balanceCol renderer False
    G.cellLayoutSetAttributes addressesCol renderer addressesModel $ \a ->
        [G.cellText := formatSingle' "{}" (address a)]
    G.cellLayoutSetAttributes balanceCol renderer addressesModel $ \a ->
        [G.cellText := show (balance a)]
    void $ G.treeViewAppendColumn treeViewAddressesView addressesCol
    void $ G.treeViewAppendColumn treeViewAddressesView balanceCol
    void $ copyAddressButton `on` G.buttonActivated $ do
        sel <- G.treeViewGetSelection treeViewAddressesView
        selNum <- G.treeSelectionCountSelectedRows sel
        rows <- G.treeSelectionGetSelectedRows sel
        when (selNum /= 0) $ do
            a <- G.listStoreGetValue addressesModel $ head $ head rows
            c <- G.clipboardGet G.selectionClipboard
            G.clipboardSetText c $ C.printPublicKey $ address a
    void $ generateAddressButton `on` G.buttonActivated $ do
        (sk, pk) <- C.keyGen
        update st $ AddAddress (C.Address pk, sk) []
        updateAddressTab st mw
    updateAddressTab st mw

updateAddressTab :: RSCoinUserState -> MainWindow -> IO ()
updateAddressTab st MainWindow{..} = do
    let AddressesTab{..} = tabAddresses
    G.listStoreClear addressesModel
    a <- getAddresses st
    forM_ a $ G.listStoreAppend addressesModel
