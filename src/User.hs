module User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import EmailAddress (EmailAddress)
import Id (Id (..))
import Protolude

data User
  = User
      { id :: Id User,
        emailAddress :: EmailAddress
      }
  deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User
