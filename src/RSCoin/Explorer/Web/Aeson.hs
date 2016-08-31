{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Types used for web communication.

module RSCoin.Explorer.Web.Aeson
       (
       ) where

-- import           Data.Aeson               (ToJSON (toEncoding, toJSON))
import           Data.Aeson.TH            (deriveToJSON)
-- import qualified Data.IntMap.Strict       as IS
-- import           GHC.Generics             (Generic)

import           Serokell.Aeson.Options   (defaultOptionsPS)

-- import qualified RSCoin.Core              as C
import           RSCoin.Core.AesonJS      ()
import           RSCoin.Explorer.Extended (CoinsMapExtension,
                                           TransactionExtension)

$(deriveToJSON defaultOptionsPS ''CoinsMapExtension)

-- I guess instance ToJSON IntMap is defined somewhere

-- -- | Newtype wrapper on top of CoinsMap which has ToJSON instance.
-- newtype SerializableCoinsMap =
--     SerializableCoinsMap C.CoinsMap
--     deriving (Show)

-- instance ToJSON SerializableCoinsMap where
--     toJSON (SerializableCoinsMap m) = toJSON . IS.assocs $ m

-- type SerCoinsMapExtended = C.WithMetadata SerializableCoinsMap CoinsMapExtension

-- mkSerCoinsMapExtended :: CoinsMapExtended -> SerCoinsMapExtended
-- mkSerCoinsMapExtended = first SerializableCoinsMap

-- instance ToJSON CoinsMapExtended where
--     toJSON = toJSON . mkSerCoinsMapExtended
--     toEncoding = toEncoding . mkSerCoinsMapExtended

$(deriveToJSON defaultOptionsPS ''TransactionExtension)
