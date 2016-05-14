{-# LANGUAGE TemplateHaskell #-}

-- | This module provides non-standart mintete configuration mechanism

module Test.RSCoin.Full.Mintette.Config
       ( MintetteConfig (..)
       , usualMintetteConfig
       ) where

import           Data.SafeCopy (base, deriveSafeCopy)

data MintetteConfig = MintetteConfig
    { inverseCheckTx       :: Bool
    , ignoreTxCheckTx      :: Bool
    , ignoreChecksCommitTx :: Bool
    , dontVerifyDpk        :: Bool
    }

usualMintetteConfig :: MintetteConfig
usualMintetteConfig = MintetteConfig False False False False

$(deriveSafeCopy 0 'base ''MintetteConfig)
