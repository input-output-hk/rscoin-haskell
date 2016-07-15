module RSCoin.Core.Crypto.Helper (parseJSONViaBytes, toJSONViaBytes) where
import           Control.Monad          ((<=<))
import           Data.Aeson             (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson.Types       as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

-- @TODO to be moved to Serokell core

parseJSONViaBytes :: (BS.ByteString -> c) -> JSON.Value -> JSON.Parser c
parseJSONViaBytes cons = either fail (return . cons) . B64.decode . encodeUtf8 <=< parseJSON

toJSONViaBytes :: (a -> BS.ByteString) -> a -> JSON.Value
toJSONViaBytes uncons = toJSON . decodeUtf8 . B64.encode . uncons

