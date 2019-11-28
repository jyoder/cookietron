module Api (Api, api, server) where

import Servant
import qualified UserApi (Api, server)

type Api = UserApi.Api

api :: Proxy Api
api = Proxy

server :: Server Api
server = UserApi.server
