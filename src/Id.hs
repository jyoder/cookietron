module Id (Id, fromText, toText, nil) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as Uuid
import Protolude

newtype Id a = Id Uuid.UUID deriving (Eq, Show, Generic)

fromText :: Text -> Maybe (Id a)
fromText text = do
  byteString <- rightToMaybe $ Base64.decode $ Text.encodeUtf8 text
  uuid <- Uuid.fromByteString $ Lazy.fromStrict byteString
  return $ Id uuid

toText :: Id a -> Text
toText (Id uuid) =
  Text.decodeUtf8
    $ Base64.encode
    $ Lazy.toStrict
    $ Uuid.toByteString uuid

nil :: Id a
nil = Id Uuid.nil

instance ToJSON (Id a)

instance FromJSON (Id a)
