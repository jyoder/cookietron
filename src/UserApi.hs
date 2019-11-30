module UserApi (Api, server) where

import qualified EmailAddress (example)
import Id (nil)
import Protolude
import Servant
import User (User (..))

type Api =
  "user" :> Get '[JSON] [User]
    :<|> "user" :> Capture "userId" Integer :> Get '[JSON] User

server :: Server Api
server = getUsers :<|> getUserById

getUsers :: Handler [User]
getUsers = return [userExample]

getUserById :: Integer -> Handler User
getUserById 0 = return userExample
getUserById _ = throwError err404

userExample :: User
userExample = User {id = nil, emailAddress = EmailAddress.example}
