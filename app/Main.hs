module Main where

import Api (api, server)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Protolude
import Servant (serve)

main :: IO ()
main = makeApp >>= runSettings (makeSettings defaultPort)

makeApp :: IO Application
makeApp = return $ serve api server

makeSettings :: Int -> Settings
makeSettings port = setPort port $ setBeforeMainLoop (showStartupMessage port) defaultSettings

showStartupMessage :: Int -> IO ()
showStartupMessage port = hPutStrLn stderr $ "Listening on port " ++ show port

defaultPort :: Int
defaultPort = 3000
