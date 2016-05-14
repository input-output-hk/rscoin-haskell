{-# LANGUAGE TemplateHaskell #-}

-- | This module provides non-standart mintete configuration mechanism

module Test.RSCoin.Full.Mintette.Config
       ( MintetteConfig (..)
       , usualMintetteConfig
       ) where

import           Data.SafeCopy (base, deriveSafeCopy)

data MintetteConfig = MintetteConfig
    {
    -- Global modificators
      checkActive                 :: Bool
    -- CheckTx modificators
    , inverseCheckTx              :: Bool
    , ignoreCheckTx               :: Bool
    , ignoreSumCheckTx            :: Bool
    , ignoreAddridInTxCheckTx     :: Bool
    , updateUtxoCheckTx           :: Bool
    -- CommitTx modificators
    , skipChecksCommitTx          :: Bool
    , skipDpkVerificationCommitTx :: Bool
    , skipOwnerCheckCommitTx      :: Bool
    , updateUtxoCommitTx          :: Bool
    }

usualMintetteConfig :: MintetteConfig
usualMintetteConfig = MintetteConfig
    { checkActive                 = True
    , inverseCheckTx              = False
    , ignoreCheckTx               = False
    , ignoreSumCheckTx            = False
    , ignoreAddridInTxCheckTx     = False
    , updateUtxoCheckTx           = True
    , skipChecksCommitTx          = False
    , skipDpkVerificationCommitTx = False
    , skipOwnerCheckCommitTx      = True
    , updateUtxoCommitTx          = True
    }


$(deriveSafeCopy 0 'base ''MintetteConfig)
