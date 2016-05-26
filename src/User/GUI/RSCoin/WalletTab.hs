{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI.RSCoin.WalletTab
       ( createWalletTab
       , initWalletTab
       , updateWalletTab
       ) where

import           Control.Monad                  (join, void)
import           Data.Maybe                     (mapMaybe)
import qualified Data.Text                      as T
import           Graphics.UI.Gtk                (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk                as G

import qualified RSCoin.Core                    as C
import           RSCoin.Timed                   (runRealMode)
import           RSCoin.User                    (RSCoinUserState)
import qualified RSCoin.User                    as U

import           Serokell.Util.Text             (format')

import           GUI.RSCoin.ExplicitTransaction (ExplicitTransaction (..),
                                                 fromTransaction,
                                                 getTransactionAmount)
import           GUI.RSCoin.Glade               (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid             (GUIState, replaceWithName)
import           GUI.RSCoin.MainWindow          (WalletModelNode (..),
                                                 WalletTab (..))
import qualified GUI.RSCoin.MainWindow          as M

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
    appendPixbufColumn model False (sid "In/out") isSendSetter
    appendTextColumn model True (sid "Status") statusSetter
    appendTextColumn model True (sid "At height") heightSetter
    appendTextColumn model True (sid "Address") addrSetter
    appendTextColumn model True (sid "Amount") amountSetter
    G.treeViewSetModel view model
    return model
  where
    sid :: String -> String
    sid = id -- overloaded strings...
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
    isSendSetter node =
        [ G.cellPixbufStockId :=
          if wIsSend node
              then sid "withdraw"
              else "deposit"]
    statusSetter node =
        [ G.cellText := case wStatus node of
              U.TxHConfirmed -> sid "Confirmed"
              U.TxHUnconfirmed -> "Unconfirmed"
              U.TxHRejected -> "Rejected" ]
    heightSetter node = [G.cellText := show (wHeight node)]
    addrSetter node = [G.cellText := wAddress node]
    amountSetter node = [G.cellText := showSigned (wAmount node)]
    showSigned a
      | a > 0 = "+" ++ show a
      | otherwise = show a

initWalletTab :: RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
initWalletTab = updateWalletTab

toNodeMapper :: RSCoinUserState
             -> GUIState
             -> U.TxHistoryRecord
             -> IO WalletModelNode
toNodeMapper st gst txhr@U.TxHistoryRecord{..} = do
    eTx <- runRealMode $ fromTransaction txhTransaction
    addrs <- runRealMode $ U.getAllPublicAddresses st
    let amountDiff = getTransactionAmount addrs eTx
        isIncome = amountDiff > 0
        headMaybe [] = Nothing
        headMaybe (x:_) = Just x
        firstFromAddress = headMaybe $ filter (`notElem` addrs) $ mapMaybe fst $ vtInputs eTx
        outputs = (map fst $ vtOutputs eTx)
        firstOutAddress = head $ filter (`notElem` addrs) outputs ++
                                 filter (`elem` addrs) outputs
    if isIncome
    then do
        (from :: T.Text) <- case firstFromAddress of
                    Nothing -> return "Unknown"
                    Just addr -> do
                        name <- replaceWithName gst addr
                        return $ format' (maybe "{}{}" (const "{} ({})") name)
                                         (name, firstOutAddress)
        return $ WalletModelNode False txhStatus txhHeight from amountDiff txhr
    else do
        (out :: T.Text) <- do
            name <- if firstOutAddress `elem` addrs
                    then return (Just "To you")
                    else replaceWithName gst firstOutAddress
            return $ format' (maybe "{}{}" (const "{} ({})") name)
                             (name, firstOutAddress)
        return $ WalletModelNode True txhStatus txhHeight out amountDiff txhr

updateWalletTab :: RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
updateWalletTab st gst M.MainWindow{..} = do
    let WalletTab{..} = tabWallet
    addrs <- runRealMode $ U.getAllPublicAddresses st
    userAmount <- runRealMode $ U.getUserTotalAmount st
    transactionsHist <- runRealMode $ U.getTransactionsHistory st
    let unconfirmed =
            filter
                (\U.TxHistoryRecord{..} -> txhStatus == U.TxHUnconfirmed)
                transactionsHist
        unconfirmedSum = do
            txs <- mapM (runRealMode . fromTransaction . U.txhTransaction)
                        unconfirmed
            return $ sum $ map (getTransactionAmount addrs) txs
    G.labelSetText labelCurrentBalance $ show $ C.getCoin userAmount
    G.labelSetText labelTransactionsNumber $ show $ length unconfirmed
    G.labelSetText labelUnconfirmedBalance . show =<< unconfirmedSum
    G.labelSetText labelCurrentAccount $ C.printPublicKey $ C.getAddress $ head addrs
    nodes <- mapM (toNodeMapper st gst) transactionsHist
    G.listStoreClear walletModel
    mapM_ (G.listStoreAppend walletModel) $ reverse nodes
    let sid = (id :: String -> String)
        dialogWithIndex i = do
            dialog <- G.dialogNew
            G.set dialog [ G.windowTitle := sid "Transaction info"]
            void $ G.dialogAddButton dialog (sid "Ok") G.ResponseOk
            upbox <- G.castToBox <$> G.dialogGetContentArea dialog
            label <- G.labelNew (Nothing :: Maybe String)
            G.set label [ G.labelText := "Information about transaction: \n" ++
                                        show (wTxHR $ reverse nodes !! i)
                        , G.labelSelectable := True
                        , G.labelWrap := True ]
            G.set upbox [ G.widgetMarginLeft := 5
                        , G.widgetMarginRight := 5
                        , G.widgetMarginTop := 5
                        , G.widgetMarginBottom := 5 ]
            G.boxPackStart upbox label G.PackGrow 10
            G.widgetShowAll upbox
            void $ G.dialogRun dialog
            G.windowResize dialog 300 500
            void $ G.widgetDestroy dialog
    void $ treeViewWallet `on` G.rowActivated $
        \l _ -> case l of
            (i:_) -> dialogWithIndex i
            _ -> return ()
    return ()
