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
                                            (.=), at, preuse)

import           Control.Monad              (forM, when)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Exception          (Exception)
import           Control.Monad.State.Lazy   (gets)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           Test.Hspec                 (Spec, describe)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Bank.Error       as B
import qualified RSCoin.Mintette.Error   as M
import qualified RSCoin.Bank.Storage     as B
import qualified RSCoin.Mintette.Storage as M
import qualified RSCoin.Core             as C

import           RSCoin.Core.Arbitrary       ()
import qualified RSCoin.Core.Storage         as T
import qualified RSCoin.Bank.StorageSpec     as B
import qualified RSCoin.Mintette.StorageSpec as M

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

-- TODO: do we have to simulate user state?
-- data UpdateToBlockHeight

data FormTransaction = FormTransaction

instance Arbitrary SomeUpdate where
    arbitrary = 
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))
            , (10, SomeUpdate <$> (arbitrary :: Gen StartNewPeriod))
            ]

instance Arbitrary RSCoinState where
    arbitrary = do
        bank <- arbitrary
        SomeUpdate upd <- arbitrary
        return . T.execUpdate (doUpdate upd) $ RSCoinState bank M.empty

liftBankUpdate :: T.Update B.BankError B.Storage a -> T.Update C.RSCoinError RSCoinState a
liftBankUpdate upd = do
    bank <- gets $ B._bankStorage . _bankState
    (res, newBank) <- T.runUpdateSafe upd bank
    bankState . B.bankStorage .= newBank
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
