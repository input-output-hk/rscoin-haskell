{-# LANGUAGE TemplateHaskell #-}

-- | This module provides non-standart mintete configuration mechanism

module Test.RSCoin.Full.Mintette.Config
       ( MintetteConfig (..)
       ) where

import           Data.SafeCopy (base, deriveSafeCopy)

data MintetteConfig = MintetteConfig
 { inverseCheckTx       :: Bool
 , ignoreTxCheckTx      :: Bool
 , ignoreChecksCommitTx :: Bool
 }

$(deriveSafeCopy 0 'base ''MintetteConfig)
