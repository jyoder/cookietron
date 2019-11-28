module Id (Id (..)) where

import Data.Aeson
import Protolude

newtype Id a = Id Integer deriving (Eq, Show, Generic)

instance ToJSON (Id a)

instance FromJSON (Id a)
