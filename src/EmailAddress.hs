module EmailAddress
  ( EmailAddress (),
    toText,
    fromText,
    example,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (String), parseJSON, toJSON, withText)
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )
import Protolude
import qualified Text.Email.Validate
  ( EmailAddress,
    toByteString,
    validate,
  )

newtype EmailAddress = EmailAddress Text.Email.Validate.EmailAddress
  deriving (Show, Eq)

toText :: EmailAddress -> Text
toText (EmailAddress emailAddress) =
  Data.Text.Encoding.decodeUtf8 $
    Text.Email.Validate.toByteString emailAddress

fromText :: Text -> Maybe EmailAddress
fromText text =
  case Text.Email.Validate.validate $ Data.Text.Encoding.encodeUtf8 text of
    Left _ -> Nothing
    Right emailAddress -> Just $ EmailAddress emailAddress

example :: EmailAddress
example = fromJust (fromText "example@email.com")

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ \text ->
    case fromText text of
      Just emailAddress -> return emailAddress
      Nothing -> empty

instance ToJSON EmailAddress where
  toJSON (EmailAddress emailAddress) = String $ toText $ EmailAddress emailAddress
