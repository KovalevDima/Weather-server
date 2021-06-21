{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ServerEnviroment(..)
    , runApp
    ) where

import           Data.Aeson
import           Network.Wai ( Application )
import           GHC.Generics ( Generic )
import           Data.Functor.Compose
import           Network.Wai.Handler.Warp ( run )
import           Servant
import           Control.Exception             (bracket, try)
import           Control.Monad                 ( forever )
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.STM             (atomically)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Control.Concurrent            (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM.TVar   (TVar, newTVar, readTVar, writeTVar, readTVarIO, newTVarIO)
import qualified Control.Monad.Catch           as E
import qualified Network.HTTP.Simple           as HTTPS
import qualified Network.HTTP.Client           as HTTPC
import qualified Data.List                     as L
import qualified Data.Time.Clock               as Time
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BS8
import qualified Data.ByteString.Lazy          as BSL

    -- Module structure:
-- 1. Run app function and config
-- 2. Server app declaration
-- 3. Server query decalration
-- 4. Cache declarations
-- 5. Block of declarations for working with the source forecast server


-- | 1. Run app function and config
data ServerEnviroment = ServerEnviroment
    { serverPort :: Int
    , locations :: [Location]
    , apikey :: APIKey
    , rootAPI :: String
    -- This option is responsible for the delay between cache fillings 
    , cacheFillingsDelay :: Int -- miliseconds
    -- This option is responsible for the time deviations between each update time of cached weather forecast and query time parameter
    , maxTimeDeviation :: Time.NominalDiffTime } -- seconds with miliseconds (for example: 5.010 - 5 seconds and 10 miliseconds)
type APIKey = String
type Location = String

runApp :: ServerEnviroment -> IO ()
runApp enviroment = do
    let port = serverPort enviroment
    cacheHolder <- initCacheHolder
    let runServer = run port (app enviroment cacheHolder)
    let fillCache = cacheFiller enviroment cacheHolder
    _ <- forkIO fillCache
    _ <- forkIO runServer
    forever $ threadDelay 100000000 -- 100 seconds
-- |



-- | 2. Server app declaration
app :: ServerEnviroment -> CacheHolder -> Application
app env cacheHolder = do
    serve api $ hoistServer api (nt cacheHolder) (server env cacheHolder)

type AppM = ReaderT CacheHolder Handler

server :: ServerEnviroment -> CacheHolder -> ServerT WeatherAPI AppM
server = getWeather
            -- Query implementation
    where   getWeather :: ServerEnviroment -> CacheHolder -> Location -> Time.UTCTime -> AppM Weather
            getWeather env cacheHolder loc date = do
                cache <- liftIO $ readCache cacheHolder
                case L.find (predicate loc date) cache of
                    Just weather -> return weather
                    Nothing      -> liftIO $ getWeatherFromSourceAPI (apikey env) loc
                    where   predicate :: Location -> Time.UTCTime -> (Weather -> Bool)
                            predicate loc time = \weather -> 
                                (city weather == loc) &&
                                abs (time `Time.diffUTCTime` requestTime weather) <= maxTimeDeviation env
 
nt :: CacheHolder -> AppM a -> Handler a
nt cacheHolder appMonad = runReaderT appMonad cacheHolder
-- | 



-- | 3. Server query decalration
api :: Proxy WeatherAPI
api = Proxy

type WeatherAPI = "weather"
    :> "city"
    :> Capture "city" String
    :> "time"
    :> Capture "time" Time.UTCTime
    :> Get '[JSON] Weather
    -- GET "localhost:port"/weather/city/*city*/date/*date*
-- |



-- | 4. Cache implementation
newtype CacheHolder = CacheHolder
    { weatherCache :: TVar [Weather]
    }

cacheFiller :: ServerEnviroment -> CacheHolder -> IO ()
cacheFiller enviroment cacheHolder = do
    weather <- mapM (getWeatherFromSourceAPI (apikey enviroment)) (locations enviroment)
    writeToCache cacheHolder weather

    putStrLn "cacheFiller worked\n" >> print "Last recorded result:" >> print weather >> putStr "\n" -- to implement normal logging!!!

    threadDelay $ cacheFillingsDelay enviroment
    cacheFiller enviroment cacheHolder

readCache :: CacheHolder -> IO [Weather]
readCache cacheHolder = do
    let CacheHolder{weatherCache = p} = cacheHolder
    readTVarIO p

writeToCache :: CacheHolder -> [Weather] -> IO ()
writeToCache cacheHolder forecastsList = do
    let CacheHolder{weatherCache = tvar} = cacheHolder
    atomically $ readTVar tvar >>= writeTVar tvar . (forecastsList ++)

initCacheHolder :: IO CacheHolder
initCacheHolder = CacheHolder <$> newTVarIO []
-- |



-- | 5. Block of declarations for working with the source forecast server
-- This function sends a request to the weather forecasts server and parses the result into the custom type
getWeatherFromSourceAPI :: APIKey -> Location -> IO Weather
getWeatherFromSourceAPI key loc =  do
    url <- HTTPC.parseUrlThrow ("http://api.weatherapi.com/v1/current.json?key=" <> key <> "&q=" <> loc <> "&aqi=no")
    response <- HTTPS.getResponseBody <$> HTTPS.httpBS url
    -- putStrLn "Response from request" >> print response >> putStr "\n" -- to implement normal logging!!!
    currentTime <- Time.getCurrentTime
    return $ Weather currentTime loc (show response)

-- The custom type
data Weather = Weather
    { requestTime :: Time.UTCTime
    , city :: Location
    , weatherForecast :: String
    } deriving (Show, Eq, Generic)

-- Parse instances
-- instance FromJSON Weather
instance ToJSON Weather where
    toEncoding = genericToEncoding defaultOptions
-- |