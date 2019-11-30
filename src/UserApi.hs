module UserApi (Api, server) where

import qualified EmailAddress (example)
import Id (nil)
import Protolude
import Servant
import User (User (..))

type Api =
  "user" :> Get '[JSON] [User]
    :<|> "user" :> Capture "userId" Text :> Get '[JSON] User
    :<|> "user" :> ReqBody '[JSON] User :> Post '[JSON] User

server :: Server Api
server = getUsers :<|> getUserById :<|> newUser

getUsers :: Handler [User]
getUsers = return [userExample]

getUserById :: Text -> Handler User
getUserById "AAAAAAAAAAAAAAAAAAAAAA==" = return userExample
getUserById _ = throwError err404

newUser :: User -> Handler User
newUser = return

userExample :: User
userExample = User {id = nil, emailAddress = EmailAddress.example}
