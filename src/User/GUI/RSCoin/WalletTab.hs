{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI.RSCoin.WalletTab
       ( createWalletTab
       , initWalletTab
       , updateWalletTab
       ) where

import           Control.Monad                  (join, void, when)
import           Data.Maybe                     (mapMaybe)
import           Data.Monoid                    ((<>))
import qualified Data.Text                      as T
import           Data.Text.Buildable            (Buildable (build))
import qualified Data.Text.Lazy.Builder         as B
import           Graphics.UI.Gtk                (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk                as G

import qualified RSCoin.Core                    as C
import           RSCoin.Timed                   (runRealMode)
import           RSCoin.User                    (RSCoinUserState,
                                                 TxHStatus (..),
                                                 TxHistoryRecord (..))
import qualified RSCoin.User                    as U

import           Serokell.Util.Text             (format', formatSingle')

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


instance Buildable TxHStatus where
    build TxHConfirmed = B.fromString "Confirmed"
    build TxHUnconfirmed = B.fromString "Unconfirmed"
    build TxHRejected = B.fromString "Rejected"

buildTransactionIndent :: C.Transaction -> B.Builder
buildTransactionIndent C.Transaction{..} =
    mconcat [ "Inputs: "
            , mconcat $ map (\x -> "\n-> " <> build x) txInputs
            , "\nOutputs: "
            , mconcat $ map (\(a,c) -> "\n<- " <> build a <>
                                       ", " <> build c) txOutputs ]

instance Buildable TxHistoryRecord where
    build TxHistoryRecord{..} =
        mconcat
            [ "Transaction: \n"
            , buildTransactionIndent txhTransaction
            , "\nAt height: "
            , build txhHeight
            , "\nStatus: "
            , build txhStatus]

initWalletTab :: RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
initWalletTab st gst mw@M.MainWindow{..} = do
    void ((M.treeViewWallet tabWallet) `on` G.rowActivated $
        \l _ -> case l of
            (i:_) -> do
                sz <- G.listStoreGetSize $ M.walletModel tabWallet
                when (i < sz) $ dialogWithIndex i
            _ -> return ())
    updateWalletTab st gst mw
  where
    sid = (id :: String -> String)
    dialogWithIndex i = do
        dialog <- G.dialogNew
        node <- G.listStoreGetValue (M.walletModel tabWallet) i
        void $ G.dialogAddButton dialog (sid "Ok") G.ResponseOk
        upbox <- G.castToBox <$> G.dialogGetContentArea dialog
        scrolled <- G.scrolledWindowNew Nothing Nothing
        label <- G.labelNew (Nothing :: Maybe String)
        G.scrolledWindowAddWithViewport scrolled label
        G.set label [ G.labelText := formatSingle'
                      "Information about transaction: \n{}" (wTxHR node)
                    , G.labelSelectable := True
                    , G.labelWrap := True ]
        G.set upbox [ G.widgetMarginLeft := 5
                    , G.widgetMarginRight := 5
                    , G.widgetMarginTop := 5
                    , G.widgetMarginBottom := 5 ]
        G.set scrolled [ G.scrolledWindowMinContentHeight := 300
                       , G.scrolledWindowMinContentWidth := 500
                       ]
        G.boxPackStart upbox scrolled G.PackNatural 0
        G.set dialog [ G.windowTitle := sid "Transaction info"
                     , G.windowResizable := False ]
        G.widgetShowAll upbox
        void $ G.dialogRun dialog
        void $ G.widgetDestroy dialog

toNodeMapper :: RSCoinUserState
             -> GUIState
             -> U.TxHistoryRecord
             -> IO WalletModelNode
toNodeMapper st gst txhr@U.TxHistoryRecord{..} = do
    eTx <- runRealMode $ fromTransaction gst txhTransaction
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
                    Nothing -> return "Emission/Fees"
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
    transactionsHist <- runRealMode $ U.getTransactionsHistory st
    userAmount <- runRealMode $ U.getUserTotalAmount False st
    let unconfirmed =
            filter
                (\U.TxHistoryRecord{..} -> txhStatus == U.TxHUnconfirmed)
                transactionsHist
        unconfirmedSum = do
            txs <- mapM (runRealMode . fromTransaction gst . U.txhTransaction)
                        unconfirmed
            return $ sum $ map (getTransactionAmount addrs) txs
    G.labelSetText labelCurrentBalance $ show $ C.getCoin userAmount
    G.labelSetText labelTransactionsNumber $ show $ length unconfirmed
    G.labelSetText labelUnconfirmedBalance . show =<< unconfirmedSum
    G.labelSetText labelCurrentAccount $ formatSingle' "{}" $ head addrs
    nodes <- mapM (toNodeMapper st gst) transactionsHist
    G.listStoreClear walletModel
    mapM_ (G.listStoreAppend walletModel) $ reverse nodes
    return ()
