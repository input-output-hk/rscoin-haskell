{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Storage.

module RSCoin.StorageSpec
       ( spec
       ) where

import           Control.Lens              (ix, makeLenses, use,
                                            (.=), at, preuse, to, (^.))

import           Control.Monad              (forM, when, forM_, unless, filterM)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.Reader       (MonadReader (ask, local))
import           Control.Exception          (Exception)
import           Control.Monad.State.Lazy   (gets)
import           Data.Int                   (Int64)
import           Data.Function              (on)
import           Data.List                  (nub, nubBy)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, fromMaybe, isJust)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Tuple.Select          (sel1)
import           Data.Typeable              (Typeable)
import           Serokell.Util.Text         (format', formatSingle')
import           Test.Hspec                 (Spec, describe)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                            frequency, Positive (..),
                                            NonEmptyList (..), NonNegative (..))

import qualified RSCoin.Bank.Error       as B
import qualified RSCoin.Mintette.Error   as M
import qualified RSCoin.Bank.Storage     as B
import qualified RSCoin.Mintette.Storage as M
import qualified RSCoin.User.Error       as U
import qualified RSCoin.User.Wallet      as U
import qualified RSCoin.User.Logic       as U
import qualified RSCoin.Core             as C

import           RSCoin.Core.Arbitrary       ()
import qualified RSCoin.Core.Storage         as T
import qualified RSCoin.Bank.StorageSpec     as B
import qualified RSCoin.Mintette.StorageSpec as M
import qualified RSCoin.User.StorageSpec     as U

spec :: Spec
spec =
    describe "Storage" $ do
        return ()

data TestError
    = TestError Text
    deriving (Show, Typeable, Eq)

instance Exception TestError

data RSCoinState =
    RSCoinState { _bankState      :: B.BankState
                , _mintettesState :: M.Map C.Mintette M.MintetteState
                -- TODO: we should probably need to have multiple user states
                , _userState      :: U.UserState
                }

$(makeLenses ''RSCoinState)

type Update = T.Update C.RSCoinError RSCoinState
type UpdateVoid = Update ()

class CanUpdate a where
    doUpdate :: a -> UpdateVoid

data SomeUpdate = forall a . CanUpdate a => SomeUpdate a

data EmptyUpdate = EmptyUpdate
    deriving Show

instance Arbitrary EmptyUpdate where
    arbitrary = pure EmptyUpdate

instance CanUpdate EmptyUpdate where
    doUpdate _ = return ()

data AddMintette = AddMintette C.Mintette (C.SecretKey, C.PublicKey) M.MintetteState
    deriving Show

instance Arbitrary AddMintette where
  arbitrary = do
    mId <- arbitrary
    mintette <- arbitrary
    sk <- arbitrary
    return $ AddMintette mId (sk, C.derivePublicKey sk) mintette

instance CanUpdate AddMintette where
    doUpdate (AddMintette mId (_, pk) mintette) = do
        liftBankUpdate $ B.addMintette mId pk
        mintettesState . at mId .= Just mintette

data StartNewPeriod = StartNewPeriod
    deriving Show

instance Arbitrary StartNewPeriod where
    arbitrary = pure StartNewPeriod

-- NOTE: this is reimplementation of Bank.Worker
instance CanUpdate StartNewPeriod where
    doUpdate _ = do
        mintettes <- use $ bankState . B.bankStorage . B.getMintettes
        pId <- use $ bankState . B.bankStorage . B.getPeriodId
        bankSk <- use $ bankState . B.bankKey
        periodResults <- forM mintettes $ 
            \mId -> do
                mSk <- preuse $ mintettesState . ix mId . M.mintetteKey
                maybe 
                    (throwM $ TestError "No mintettes secret key") 
                    (liftMintetteUpdate mId . flip M.finishPeriod pId) 
                    mSk
        newPeriodData <- liftBankUpdate . B.startNewPeriod bankSk $ map Just periodResults
        newMintettes <- use $ bankState . B.bankStorage . B.getMintettes
        mapM_
            (\(m,mId) -> do
                    when (length newPeriodData < mId) $
                        throwM $ TestError "No such mintette"
                    liftMintetteUpdate m $ M.startPeriod (newPeriodData !! mId))
            (zip newMintettes [0 ..])

data UpdateToBlockHeight = UpdateToBlockHeight
    deriving Show

instance Arbitrary UpdateToBlockHeight where
    arbitrary = pure UpdateToBlockHeight

-- NOTE: this is reimplementation of User.Actions.updateToBlockHeight
instance CanUpdate UpdateToBlockHeight where
    doUpdate _ = do
        walletHeight <- liftUserUpdate U.getLastBlockId
        lastBlockHeight <- use $ bankState . B.bankStorage . B.getPeriodId . to pred
        when (walletHeight > lastBlockHeight) $
            throwM $
            U.StorageError $
            U.InternalError $
            format'
                ("Last block height in wallet ({}) is greater than last " <>
                 "block's height in bank ({}). Critical error.")
                (walletHeight, lastBlockHeight)
        when (lastBlockHeight /= walletHeight) $
            forM_
                [walletHeight + 1 .. lastBlockHeight]
                updateToBlockHeight
      where
        updateToBlockHeight newHeight = do
            C.HBlock {..} <- use $ bankState . B.bankStorage . B.getHBlock newHeight . to fromJust
            liftUserUpdate $ U.withBlockchainUpdate newHeight hbTransactions

-- TODO: add endpoint for testing addAddress also

type InvalidAddress = Maybe C.Address
type ValidAddressIndex = NonNegative Int
type ToAddressIndex = ValidAddressIndex
type FromAddresses = NonEmptyList (ValidAddressIndex, Positive Int64)

data FormTransaction = FormTransaction InvalidAddress FromAddresses ToAddressIndex
    deriving Show

instance Arbitrary FormTransaction where
    arbitrary = FormTransaction <$> arbitrary <*> arbitrary <*> arbitrary

-- NOTE: this is reimplementation of User.Actions.formTransaction
instance CanUpdate FormTransaction where
    doUpdate (FormTransaction invalidAddress (getNonEmpty -> fromIndexes) toIndex) = do
        -- Generating random transaction
        -- TODO: it would be useful if I could do arbitrary in this monad
        publicAddresses <- liftUserUpdate U.getPublicAddresses
        when (null publicAddresses) $
            throwM $ TestError "No public addresses"
        let validAddress = getAddress publicAddresses toIndex
            inputs = map (\(a, b) -> (getNonNegative a, getPositive b)) fromIndexes
            outputAddr = fromMaybe validAddress invalidAddress
            outputCoin = C.Coin . sum $ map snd inputs
        when
            (nubBy ((==) `on` fst) inputs /= inputs) $
            commitError "All input addresses should have distinct IDs."
        unless (all (> 0) $ map snd inputs) $
            commitError $
            formatSingle'
                "All input values should be positive, but encountered {}, that's not." $
            head $ filter (<= 0) $ map snd inputs
        accounts <- liftUserUpdate U.getAllAddresses
        let notInRange i = i <= 0 || i > length accounts
        when
            (any notInRange $ map fst inputs) $
            commitError $
            format'
                "Found an account id ({}) that's not in [1..{}]"
                ( head $ filter notInRange $ map fst inputs
                , length accounts)
        let accInputs :: [(Int, U.UserAddress, C.Coin)]
            accInputs = map (\(i,c) -> (i, accounts !! (i - 1), C.Coin c)) inputs
            hasEnoughFunds (i,acc,c) = liftUserUpdate $ do
                amount <- getAmount acc
                return $
                    if amount >= c
                        then Nothing
                        else Just i
        overSpentAccounts <-
            filterM (\a -> isJust <$> hasEnoughFunds a) accInputs
        unless (null overSpentAccounts) $
            commitError $
            (if length overSpentAccounts > 1
                then "At least the"
                else "The") <>
            formatSingle'
                " following account doesn't have enough coins: {}"
                (sel1 $ head overSpentAccounts)
        (addrPairList,outTr) <- liftUserUpdate $
            foldl1 mergeTransactions <$> mapM (formTransactionMapper outputAddr outputCoin) accInputs
        walletHeight <- liftUserUpdate U.getLastBlockId
        lastBlockHeight <- use $ bankState . B.bankStorage . B.getPeriodId . to pred
        when (walletHeight /= lastBlockHeight) $
            commitError $
            format'
                ("Wallet isn't updated (lastBlockHeight {} when blockchain's last block is {}). " <>
                "Please synchonize it with blockchain. The transaction wouldn't be sent.")
                (walletHeight, lastBlockHeight)
        let signatures =
                M.fromList $
                map (\(addrid',address') ->
                        (addrid', C.sign (address' ^. U.privateAddress) outTr))
                    addrPairList
        validateTransaction outTr signatures $ lastBlockHeight + 1
        liftUserUpdate $ U.addTemporaryTransaction outTr
      where
        getAmount userAddress =
            sum . map (C.getAmountByAddress $ U.toAddress userAddress) <$> U.getTransactions userAddress
        commitError = throwM . U.InputProcessingError
        getAddress [] _ = error "This should not happen"
        getAddress list index = C.Address $ list !! (getNonNegative index `mod` length list)
        formTransactionMapper :: C.Address
                              -> C.Coin
                              -> (Int, U.UserAddress, C.Coin)
                              -> T.Update U.UserError U.WalletStorage ([(C.AddrId, U.UserAddress)], C.Transaction)
        formTransactionMapper outputAddr outputCoin (_,a,c) = do
            (addrids :: [C.AddrId]) <-
                concatMap (C.getAddrIdByAddress $ U.toAddress a) <$>
                U.getTransactions a
            let (chosen,leftCoin) = C.chooseAddresses addrids c
                transaction =
                    C.Transaction chosen $
                    (outputAddr, outputCoin) :
                    (if leftCoin == 0
                        then []
                        else [(U.toAddress a, leftCoin)])
                addrPairList = map (, a) chosen
            return (addrPairList, transaction)
        mergeTransactions
            :: ([(C.AddrId, U.UserAddress)], C.Transaction)
            -> ([(C.AddrId, U.UserAddress)], C.Transaction)
            -> ([(C.AddrId, U.UserAddress)], C.Transaction)
        mergeTransactions (s1,a) (s2,b) =
            ( nub $ s1 <> s2
            , C.Transaction
                (C.txInputs a <> C.txInputs b)
                (nub $ C.txOutputs a <> C.txOutputs b))

validateTransaction :: C.Transaction -> M.Map C.AddrId C.Signature -> C.PeriodId -> UpdateVoid
validateTransaction tx@C.Transaction{..} signatures height = do
    undefined
    -- FIXME: now I should reimplement everything from User.Logic module
    -- there will be few more these functions, like Mintette.Worker, and maybe some other that I am not aware of. I don't like this approach, because lots of logic has been reimplemented. Ideally I should reuse these `WorkMode` functions and run them with pure monad stack `PureRpc` and `TimedT`. What do you think? Or should I just continue and reimplement User.Logic (plus some extra stuff from Core.Communication) and Mintette.Worker.

instance Arbitrary SomeUpdate where
    arbitrary = 
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))
            , (10, SomeUpdate <$> (arbitrary :: Gen StartNewPeriod))
            , (10, SomeUpdate <$> (arbitrary :: Gen UpdateToBlockHeight))
            , (10, SomeUpdate <$> (arbitrary :: Gen FormTransaction))
            ]

instance Arbitrary RSCoinState where
    arbitrary = do
        bank <- arbitrary
        user <- arbitrary
        SomeUpdate upd <- arbitrary
        return . T.execUpdate (doUpdate upd) $ RSCoinState bank M.empty user

-- TODO: there must be a better way to write this (in more lens style)
liftBankUpdate :: T.Update B.BankError B.Storage a -> T.Update C.RSCoinError RSCoinState a
liftBankUpdate upd = do
    bank <- gets $ B._bankStorage . _bankState
    (res, newBank) <- T.runUpdateSafe upd bank
    bankState . B.bankStorage .= newBank
    return res

liftUserUpdate :: T.Update U.UserError U.WalletStorage a -> T.Update C.RSCoinError RSCoinState a
liftUserUpdate upd = do
    user <- gets $ U._userStorage . _userState
    (res, newUser) <- T.runUpdateSafe upd user
    userState . U.userStorage .= newUser
    return res

liftMintetteUpdate :: C.Mintette -> T.Update M.MintetteError M.Storage a -> T.Update C.RSCoinError RSCoinState a
liftMintetteUpdate mintette upd = do
    mMintette <- gets (fmap M._mintetteStorage . M.lookup mintette . _mintettesState)
    mRes <- return $ mMintette >>= T.runUpdateSafe upd 
    maybe (throwM $ TestError "No mintette") updateMintette mRes
    -- FIXME: return Maybe a instead of error ?
  where
    updateMintette (ret, newStorage) = do
        mintettesState . ix mintette . M.mintetteStorage .= newStorage
        return ret
