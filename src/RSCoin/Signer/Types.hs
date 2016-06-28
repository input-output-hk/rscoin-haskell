module RScoin.Signer.Types
  ( Response(..)
  , AsyncResponse(..)
  ) where

import qualified RSCoin.Core as C

data Response = Approved C.Signature | Rejected
data AsyncResponse = Ready Response | Await
