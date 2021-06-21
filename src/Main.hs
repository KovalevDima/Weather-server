{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Lib ( runApp, ServerEnviroment(..) )


env :: ServerEnviroment
env = ServerEnviroment
        { serverPort = 8080
        , locations = ["London", "Boston"]
        , apikey = ""
        , rootAPI = "weather"
        , cacheFillingsDelay = 30 * 1000000  -- miliseconds
        , maxTimeDeviation = 50 -- seconds
        }

main :: IO ()
main = do
        runApp env
