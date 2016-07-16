module RSCoin.Core.Crypto.Helper (parseJSONBase64, toJSONBase64) where
import           Control.Monad          ((<=<))
import           Data.Aeson             (FromJSON (parseJSON), ToJSON (toJSON))
import qualified Data.Aeson.Types       as JSON
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)

-- @TODO to be moved to Serokell core

parseJSONBase64 :: JSON.Value -> JSON.Parser BS.ByteString
parseJSONBase64 = either fail return . B64.decode . encodeUtf8 <=< parseJSON

toJSONBase64 :: BS.ByteString -> JSON.Value
toJSONBase64 = toJSON . decodeUtf8 . B64.encode

