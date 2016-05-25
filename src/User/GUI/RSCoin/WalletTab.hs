{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

module GUI.RSCoin.WalletTab
       ( createWalletTab
       , initWalletTab
       , updateWalletTab
       ) where

import           Control.Monad         (join, void)
import           Graphics.UI.Gtk       (AttrOp ((:=)))
import qualified Graphics.UI.Gtk       as G

import qualified RSCoin.Core           as C
import           RSCoin.Timed          (runRealMode)
import           RSCoin.User           (RSCoinUserState)
import qualified RSCoin.User           as U

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid    (GUIState)
import           GUI.RSCoin.MainWindow (WalletModelNode (..), WalletTab (..))
import qualified GUI.RSCoin.MainWindow as M

type Model = G.ListStore WalletModelNode

createWalletTab :: GladeMainWindow -> IO WalletTab
createWalletTab GladeMainWindow{..} = do
    makeHeaderRed
    containerSeparatorColor gBoxRSCoinLogo redColor
    containerSeparatorColor gBoxWalletHeader whiteColor
    model <- createWalletModel gTreeViewWallet
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
    makeHeaderRed = do
        G.widgetModifyBg gBoxWalletHeaderWrapper G.StateNormal redColor
        G.widgetModifyFg gBoxWalletHeaderWrapper G.StateNormal whiteColor
    containerSeparatorColor widget color =
        join $
        mapM_
            (\w ->
                  G.widgetModifyBg w G.StateNormal color) .
        map snd .
        filter (odd . fst) . zip [(0 :: Integer) ..] <$>
        G.containerGetChildren widget
    whiteColor = G.Color 65535 65535 65535
    redColor = G.Color 52685 9252 9252
-- What's that? Not used anywhere.
--    addStyle widget css' = do
--        ctx <- G.widgetGetStyleContext widget
--        css <- cssProviderGetDefault
--        cssProviderLoadFromString css css'
--        styleContextAddProvider ctx css 1

createWalletModel :: G.TreeView -> IO Model
createWalletModel view = do
    model <- G.listStoreNew []
    appendPixbufColumn model True "Status" statusSetter
    appendTextColumn model True "Confirmation" confirmationSetter
    appendTextColumn model True "At height" heightSetter
    appendTextColumn model True "Address" addrSetter
    appendTextColumn model True "Amount" amountSetter
    G.treeViewSetModel view model
    return model
  where
    appendPixbufColumn model expand title attributesSetter = do
        renderer <- G.cellRendererPixbufNew
        appendColumn renderer model expand title attributesSetter
    appendTextColumn model expand title attributesSetter = do
        renderer <- G.cellRendererTextNew
        appendColumn renderer model expand title attributesSetter
    appendColumn renderer model expand title attributesSetter = do
        column <- G.treeViewColumnNew
        G.treeViewColumnSetTitle column title
        G.treeViewColumnSetExpand column expand
        G.cellLayoutPackStart column renderer False
        G.cellLayoutSetAttributes column renderer model attributesSetter
        void $ G.treeViewAppendColumn view column
    statusSetter node =
        [ G.cellPixbufStockId :=
          if wIsSend node
              then "withdraw"
              else "deposit"]
    confirmationSetter node =
        [ G.cellText :=
          if wIsConfirmed node
              then "Confirmed"
              else "Unconfirmed"]
    heightSetter node = [G.cellText := show (wHeight node)]
    addrSetter node = [G.cellText := wAddress node]
    amountSetter node = [G.cellText := showSigned (wAmount node)]
    showSigned a
      | a > 0 = "+" ++ show a
      | otherwise = show a

initWalletTab :: RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
initWalletTab = updateWalletTab

toNodeMapper :: U.TxHistoryRecord -> IO WalletModelNode
toNodeMapper U.TxHistoryRecord{..} =
    -- FIXME it's a stub, fix using cast to ExplicitTransaction,...
    return $ WalletModelNode False False 123 "AOEUAOEUAOEUAOEU" 45

updateWalletTab :: RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
updateWalletTab st _ M.MainWindow{..} = do
    let WalletTab{..} = tabWallet
    userAmount <- runRealMode $ U.getUserTotalAmount st
    G.labelSetText labelCurrentBalance $ show $ C.getCoin userAmount
    transactionsHist <- runRealMode $ U.getTransactionsHistory st
    let unconfirmed =
            filter
                (\U.TxHistoryRecord{..} -> txhStatus == U.TxHUnconfirmed)
                transactionsHist
    G.labelSetText labelTransactionsNumber $ show $ length unconfirmed
    nodes <- mapM toNodeMapper transactionsHist
    G.listStoreClear walletModel
    mapM_ (G.listStoreAppend walletModel) nodes
