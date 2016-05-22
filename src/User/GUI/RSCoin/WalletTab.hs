{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module GUI.RSCoin.WalletTab (initWalletTab) where

import           Control.Lens          ((^.))
import           Control.Monad         (void)

import           Graphics.UI.Gtk       (AttrOp ((:=)))
import qualified Graphics.UI.Gtk       as G

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import qualified GUI.RSCoin.MainWindow as M

type Model = G.ListStore M.WalletModelNode

initWalletTab :: GladeMainWindow -> IO M.WalletTab
initWalletTab GladeMainWindow{..} = do
    model <- createRandomWalletModel gTreeViewWallet
    addRandomData model
    return $
        M.WalletTab
            gTreeViewWallet
            model
            gBoxWalletHeader
            gLabelCurrentBalance
            gLabelUnconfirmedBalance
            gLabelTransactionsNumber
            gLabelCurrentAccount

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
          if node ^. M.wIsSend
              then "Out"
              else "In"]
    confirmationSetter node =
        [ G.cellText :=
          if node ^. M.wIsConfirmed
              then "Confirmed"
              else "Unconfirmed"]
    timeSetter node = [G.cellText := node ^. M.wTime]
    addrSetter node = [G.cellText := node ^. M.wAddress]
    amountSetter node = [G.cellText := showSigned (node ^. M.wAmount)]
    showSigned a
      | a > 0 = "+" ++ show a
      | otherwise = show a

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
        return $ M.WalletModelNode st1 st2 tm addr am
