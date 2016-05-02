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
                                            (.=), at, preuse, to)

import           Control.Monad              (forM, when, forM_)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.Reader       (MonadReader (ask, local))
import           Data.Monoid                ((<>))
import           Control.Exception          (Exception)
import           Control.Monad.State.Lazy   (gets)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text)
import           Serokell.Util.Text         (format', formatSingle')
import           Data.Typeable              (Typeable)
import           Test.Hspec                 (Spec, describe)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Bank.Error       as B
import qualified RSCoin.Mintette.Error   as M
import qualified RSCoin.Bank.Storage     as B
import qualified RSCoin.Mintette.Storage as M
import qualified RSCoin.User.Error       as U
import qualified RSCoin.User.Wallet      as U
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

instance Arbitrary SomeUpdate where
    arbitrary = 
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))
            , (10, SomeUpdate <$> (arbitrary :: Gen StartNewPeriod))
            , (10, SomeUpdate <$> (arbitrary :: Gen UpdateToBlockHeight))
            ]

instance Arbitrary RSCoinState where
    arbitrary = do
        bank <- arbitrary
        user <- arbitrary
        SomeUpdate upd <- arbitrary
        return . T.execUpdate (doUpdate upd) $ RSCoinState bank M.empty user

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
