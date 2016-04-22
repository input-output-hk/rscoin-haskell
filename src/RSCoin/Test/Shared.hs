
module Shared 
    ( 

    ) where

import          Control.Concurrent.STM.TVar    (newTVar, writeTVar, readTVar
                                               , modifyTVar)

-- | Same as MonadState, but with a little difference in semantics.
--   It keeps a reference to the state rather than state itself.
class (MonadState s, MonadIO m) => MonadShared a s m | m -> s, a -> s where
    getActial :: m a

    putActual :: a -> m ()



    
