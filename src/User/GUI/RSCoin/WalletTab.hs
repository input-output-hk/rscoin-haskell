{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module GUI.RSCoin.WalletTab
       ( createWalletTab
       , initWalletTab
       ) where

import           Control.Monad                        (void)

import           Graphics.UI.Gtk                      (AttrOp ((:=)))
import qualified Graphics.UI.Gtk                      as G
import           Graphics.UI.Gtk.General.CssProvider  (cssProviderGetDefault, cssProviderLoadFromString)
import           Graphics.UI.Gtk.General.StyleContext (styleContextAddProvider)

import           GUI.RSCoin.Glade                     (GladeMainWindow (..))
import           GUI.RSCoin.MainWindow                (WalletModelNode (..), WalletTab (..))
import qualified GUI.RSCoin.MainWindow                as M

type Model = G.ListStore WalletModelNode

createWalletTab :: GladeMainWindow -> IO WalletTab
createWalletTab GladeMainWindow{..} = do
    addStyle
    model <- createRandomWalletModel gTreeViewWallet
    return $
        WalletTab
            gTreeViewWallet
            model
            gBoxWalletHeader
            gLabelCurrentBalance
            gLabelUnconfirmedBalance
            gLabelTransactionsNumber
            gLabelCurrentAccount
  where
    customCss = "* { background-color: #ff0000; }"
    addStyle  = do
        ctx <- G.widgetGetStyleContext gBoxWalletHeader
        css <- cssProviderGetDefault
        cssProviderLoadFromString css customCss
        styleContextAddProvider ctx css 1

createRandomWalletModel :: G.TreeView -> IO Model
createRandomWalletModel view = do
    model <- G.listStoreNew []
    appendColumn model True "Status" statusSetter
    appendColumn model True "Confirmation" confirmationSetter
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
    statusSetter node =
        [ G.cellText :=
          if wIsSend node
              then "Out"
              else "In"]
    confirmationSetter node =
        [ G.cellText :=
          if wIsConfirmed node
              then "Confirmed"
              else "Unconfirmed"]
    timeSetter node = [G.cellText := wTime node]
    addrSetter node = [G.cellText := wAddress node]
    amountSetter node = [G.cellText := showSigned (wAmount node)]
    showSigned a
      | a > 0 = "+" ++ show a
      | otherwise = show a

initWalletTab :: M.MainWindow -> IO ()
initWalletTab M.MainWindow{..} = addRandomData $ walletModel tabWallet

addRandomData :: Model -> IO ()
addRandomData model = mapM_ (G.listStoreAppend model) randomModelData
  where
    randomModelData = do
        tm <- ["2:20", "6:42", "12:31"]
        st2 <- [True, False]
        am <- [123, -3456, 12345, -45323]
        st1 <- [True, False]
        addr <- [ "A7FUZi67YbBonrD9TrfhX7wnnFxrIRflbMFOpI+r9dOc"
                , "G7FuzI67zbBbnrD9trfh27anNf2RiRFLBmfBPi+R9DBC"
                ]
        return $ WalletModelNode st1 st2 tm addr am
