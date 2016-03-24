{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState

module AcidState
       ( State
       , openState
       , closeState
       , AddMintette (..)
       ) where

import           Data.Acid               (AcidState, closeAcidState, makeAcidic,
                                          openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           Serokell.Util.AcidState (stateToUpdate)

import qualified Storage                 as BS

type State = AcidState BS.Storage

$(deriveSafeCopy 0 'base ''BS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

addMintette m = stateToUpdate . BS.addMintette m

$(makeAcidic ''BS.Storage
             [ 'addMintette
             ])
