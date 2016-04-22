
-- | This module creates an instance of MonadBaseControl for newtypes
module RSCoin.Test.NewtypeBaseControl 
    () where

{-
import Control.Monad.Trans.Control (MonadBaseControl, Stm 
                                  , liftBaseWith, restoreM)
import Control.Newtype            (Newtype, pack, unpack)

instance (MonadBaseControl IO o, Newtype n o) => MonadBaseControl IO n where
    type StM n a  =  a
 
    liftBaseWith f  =  pack $ liftBaseWith $ \g -> f $ g . unpack

    restoreM  =  pack . restoreM
-}

