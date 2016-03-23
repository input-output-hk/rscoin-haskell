{-# LANGUAGE TemplateHaskell #-}

-- | Wrap Storage into AcidState

module AcidState
       ( State
       , openState
       , closeState
       ) where

import           Data.Acid     (AcidState, closeAcidState, makeAcidic,
                                openLocalStateFrom)
import           Data.SafeCopy (base, deriveSafeCopy)

import qualified Storage       as BS

type State = AcidState BS.Storage

$(deriveSafeCopy 0 'base ''BS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

$(makeAcidic ''BS.Storage
             [
             ])
