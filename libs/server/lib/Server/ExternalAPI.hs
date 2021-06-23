{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.ExternalAPI ( Weather(..), getWeatherFromSourceAPI, Location, APIKey ) where
        
import           Data.Aeson                    ( (.:), withObject, defaultOptions, genericToEncoding, FromJSON(parseJSON), ToJSON(toEncoding) )
import           System.Exit                   ( die )
import           GHC.Generics                  ( Generic )
import qualified Control.Exception             as E
import qualified Data.Time.Clock.POSIX         as Time
import qualified Network.HTTP.Simple           as HTTPS
import qualified Network.HTTP.Client           as HTTPC

import           Config                        ( Location, APIKey )


-- This function sends a request to the weather forecasts server and parses the result into the custom type
getWeatherFromSourceAPI :: APIKey -> Location -> IO Weather
getWeatherFromSourceAPI key loc =  do
    url <- HTTPC.parseUrlThrow ("http://api.weatherapi.com/v1/current.json?key=" <> key <> "&q=" <> loc <> "&aqi=no")
    fetchForecast url

fetchForecast :: HTTPC.Request -> IO Weather
fetchForecast url = 
    E.catch
        (HTTPS.getResponseBody <$> HTTPS.httpJSON url) 
        (\(e :: E.IOException) -> die $ "Something unexpected went wrong in function fetchJSON\nError message:" <> show e)

data Weather = Weather
    { city :: Location
    , requestTime :: Time.POSIXTime
    , forecast :: String
    } deriving (Show, Eq, Generic)

-- Parse instances
instance FromJSON Weather where
     parseJSON = withObject "Weather" $ \v -> do
            -- city
            location <- v .: "location"
            city <- location .: "name"
            -- time
            current <- v .: "current"
            requestTime <- current .: "last_updated_epoch"
            -- forecast
            condition <- current .: "condition"
            forecast <- condition .: "text"

            return Weather{..}

instance ToJSON Weather where
    toEncoding = genericToEncoding defaultOptions