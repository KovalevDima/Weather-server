{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}
module Lib
    ( ServerEnviroment(..)
    , runServer
    ) where
            
import Data.Aeson ()
import Network.Wai ( Application )
import GHC.Generics ()
import GHC.TypeLits ()
import Network.Wai.Handler.Warp ( run )
import Servant
import Servant.Client

-- | Imports of this module
runServer :: Int -> ServerEnviroment -> IO ()
runServer port enviroment = do
    putStrLn "Server starting.."
    run port (app enviroment)


data ServerEnviroment = ServerEnviroment
  { port :: Int
  , locations :: [Location]
  , apikey :: String
  , rootAPI :: String
  }
-- |


-- | Server
server :: ServerEnviroment -> Server WeatherAPI
server enviroment = \cityname date -> getWeather cash cityname date (apikey enviroment)
    where cash = undefined

app :: ServerEnviroment -> Application
app enviroment = serve api (server enviroment)
-- |


-- | Server API
api :: Proxy WeatherAPI
api = Proxy

type WeatherAPI =     
    "weather" :> "cityname" :> Capture "cityname" Location :> "date" :> Capture "date" String :> Get '[JSON] Weather -- GET /weather/cityname/*city*/date/*date*
-- |


-- This function checks the cash for inforomation about weather 
-- and returns this information if it's exists, otherwise
-- it perform an API request
getWeather :: Cash -> Location -> Date -> APIKey -> Handler Weather
getWeather cash loc date key =
    case checkCashForWeatherInCity cash loc date of
        Just weather -> return weather
        Nothing -> do getWeatherFromAPI loc date key

getWeatherFromAPI :: Location -> Date -> APIKey -> Handler Weather
getWeatherFromAPI loc date key = do
    return "Good weather"

checkCashForWeatherInCity :: Cash -> Location -> Date -> Maybe Weather
checkCashForWeatherInCity cash city date = Nothing


type Weather = String
type Location = String
type APIKey = String
type Date = String
type Cash = [Weather]