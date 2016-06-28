{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module generates Acidic instance. It's separated from the
-- main AcidState only in this way extending AcidState is possible
-- (and it's done in testing framework)

module RSCoin.Signer.Acidic
       ( openState
       , openMemState
       , closeState
       , SignTx (..)
       , GetSignedTx (..)
       ) where

import           Data.Acid                 (closeAcidState, makeAcidic,
                                            openLocalStateFrom)
import           Data.Acid.Memory          (openMemoryState)

import           RSCoin.Signer.AcidState (State)
import qualified RSCoin.Signer.AcidState as S
import qualified RSCoin.Signer.Storage   as SS

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp SS.mkStorage

openMemState :: IO State
openMemState = openMemoryState SS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

$(makeAcidic ''SS.Storage
             [ 'S.signTx
             , 'S.getSignedTx
             ])
