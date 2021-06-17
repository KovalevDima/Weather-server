{-# LANGUAGE RecordWildCards #-}
module Main where
import Lib


env :: ServerEnviroment
env = ServerEnviroment
        { port = 8080
        , locations = []
        , apikey = ""
        , rootAPI = ""
        }

main :: IO ()
main = do
        runServer 8080 env