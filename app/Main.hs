module Main where

import Api (api, server)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Protolude
import Servant (serve)

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $ setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve api server

main :: IO ()
main = Main.run
