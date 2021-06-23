{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib 
    ( runApp
    , configurate
    ) where

import           Data.Aeson
import           Network.Wai                   ( Application )
import           GHC.Generics                  ( Generic )
import           Network.Wai.Handler.Warp      ( run )
import           Servant
import           System.Exit                   ( die )
import           System.Directory              ( getCurrentDirectory )    
import           Control.Exception             (bracket, try)
import           Control.Monad                 ( forever )
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.STM             (atomically)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Control.Concurrent            (forkIO, killThread, threadDelay)
import           Control.Concurrent.STM.TVar   (TVar, newTVar, readTVar, writeTVar, readTVarIO, newTVarIO)
import qualified Control.Exception             as E
import qualified Network.HTTP.Simple           as HTTPS
import qualified Network.HTTP.Client           as HTTPC
import qualified Data.List                     as L
import qualified Data.Time                     as Time
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString               as BS

    -- Module structure:
-- 1. Run app function and config
-- 2. Server app declaration
-- 3. Server query declaration
-- 4. Cache impementation
-- 5. Block of declarations for working with the source forecast server
-- 6. Config and configuration functions
-- 7. Logger



-- | 1. Run app function
runApp :: ServerEnviroment -> IO ()
runApp enviroment = do
    let port = serverPort enviroment
    cacheHolder <- initCacheHolder
    let runServer = logInfo "Server starts" >> run port (app enviroment cacheHolder)
    let fillCache = logInfo "Cash filler starts" >> cacheFiller enviroment cacheHolder
    _ <- forkIO fillCache
    _ <- forkIO runServer
    forever $ threadDelay 1800000000
-- |



-- | 2. Server app declaration
app :: ServerEnviroment -> CacheHolder -> Application
app env cacheHolder = do
    serve api $ hoistServer api (nt cacheHolder) (server env cacheHolder)

type AppM = ReaderT CacheHolder Handler

server :: ServerEnviroment -> CacheHolder -> ServerT WeatherAPI AppM
server = getWeather
            -- Query implementation
    where   getWeather :: ServerEnviroment -> CacheHolder -> Location -> Time.POSIXTime -> AppM Weather
            getWeather env cacheHolder loc date = do
                cache <- liftIO $ readCache cacheHolder
                case L.find (predicate env loc date) cache of
                    Just weather -> liftIO $ logDebug ("" <> show weather <> " found in cash") >> return weather
                    Nothing      -> liftIO $ logDebug "Didn't found anything in cash" >> getWeatherFromSourceAPI (apikey env) loc

predicate ::  ServerEnviroment -> Location -> Time.POSIXTime -> (Weather -> Bool)
predicate env loc time = \weather -> 
    (city weather == loc) &&
    abs (time - requestTime weather) <= fromIntegral (maxDeviation env)

nt :: CacheHolder -> AppM a -> Handler a
nt cacheHolder appMonad = runReaderT appMonad cacheHolder
-- | 



-- | 3. Server query declaration
api :: Proxy WeatherAPI
api = Proxy  -- ("root" :> WeatherAPI)

newtype (WeatherAPI' a) = WeatherAPI' (a :> WeatherAPI)
type WeatherAPI = "weather"
    :> "city"
    :> Capture "city" String
    :> "time"
    :> Capture "time" Time.POSIXTime
    :> Get '[JSON] Weather
    -- GET "localhost:8080/weather/city/*city*/time/*time*
-- |



-- | 4. Cache implementation
newtype CacheHolder = CacheHolder
    { weatherCache :: TVar [Weather]
    }

cacheFiller :: ServerEnviroment -> CacheHolder -> IO ()
cacheFiller enviroment cacheHolder = do
    weather <- mapM (getWeatherFromSourceAPI (apikey enviroment)) (locations enviroment)
    writeToCache cacheHolder weather
    logInfo $ show (length weather) <> " weather forecasts added to cache"
    cash <- readCache cacheHolder
    logDebug $ "Current cash state: " <> show cash
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
    fetchJSON url

fetchJSON :: HTTPC.Request -> IO Weather
fetchJSON url = 
    E.catch
        (HTTPS.getResponseBody <$> HTTPS.httpJSON url) 
        (\(e :: E.IOException) -> die $ "Something unexpected went wrong in function fetchJSON\nError message" <> show e)

-- The custom type
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
-- |



-- | 6. Config and configuration functions
data ServerEnviroment = ServerEnviroment
    { serverPort :: Int
    , apikey :: APIKey
    , locations :: [Location]
    -- This option is responsible for the delay between cache fillings 
    , cacheFillingsDelay :: Int -- miliseconds
    -- This option is responsible for the time deviations between each update time of cached weather forecast and query time parameter
    , maxDeviation :: Int -- seconds
    } deriving (Show, Eq, Generic) 

instance FromJSON ServerEnviroment
instance ToJSON ServerEnviroment where
    toEncoding = genericToEncoding defaultOptions

configurate :: IO ServerEnviroment
configurate = do
    cfg <- E.catch
        readConfig 
        (\(e :: E.IOException) -> putStrLn "Config file will be created in the current directory" >> createConfigFile >> readConfig)
    putStrLn "Config was loaded from current directory"
    E.catch -- Authorization checking
        (performRequest cfg)
        (\(e :: HTTPC.HttpException) -> putStrLn "Autorization failed" >>  changeKey cfg >>= writeToConfigFile >> configurate)

performRequest :: ServerEnviroment -> IO ServerEnviroment
performRequest cfg = do
    HTTPC.parseUrlThrow ("http://api.weatherapi.com/v1/timezone.json?key=" <> apikey cfg <> "&q=London") >>= HTTPS.httpBS
    return cfg

readConfig :: IO ServerEnviroment
readConfig = do
    directory <- getCurrentDirectory
    config <- BS.readFile (directory <> "/config.yaml")
    Yaml.decodeThrow config

createConfigFile :: IO ()
createConfigFile = do
    putStrLn "Default config was created. You can change it in config.yaml"
    config <- changeKey env
    writeToConfigFile config
    putStrLn "Enter any key to load config"
    _ <- getLine
    return ()

writeToConfigFile :: ServerEnviroment -> IO ()
writeToConfigFile config = do
    directory <- getCurrentDirectory
    let content = Yaml.encode config
    BS.writeFile (directory <> "/config.yaml") content

changeKey :: ServerEnviroment -> IO ServerEnviroment
changeKey config = do
    putStrLn "Enter the apikey"
    key <- getLine
    return config{apikey = key}

env :: ServerEnviroment
env = ServerEnviroment
    { serverPort = 8080
    , locations = ["London", "Boston"]
    , apikey = ""
    , cacheFillingsDelay = 90 * 1000000  -- miliseconds
    , maxDeviation = 3600 -- seconds
    }

type APIKey = String
type Location = String
-- |



-- | 7. Logger
data Logger = Logger
    { logInfoToConsole :: String -> IO ()
    , logWarnToConsole :: String -> IO ()
    , logDebugToConsole :: String -> IO ()
    }

serverLogger :: Logger
serverLogger = Logger
    { logInfoToConsole = \info -> do
        show <$> Time.getCurrentTime >>= \time -> putStrLn $ "  Info  | " <> take 19 time <> " | " <> info
    , logWarnToConsole = \warn -> do
        show <$> Time.getCurrentTime >>= \time -> putStrLn $ "  Warn  | " <> take 19 time <> " | " <> warn
    , logDebugToConsole = \debug -> do
        show <$> Time.getCurrentTime >>= \time -> putStrLn $ "  Debug | " <> take 19 time <> " | " <> debug
    }

logInfo :: String -> IO ()
logInfo = logInfoToConsole serverLogger

logWarn :: String -> IO ()
logWarn = logWarnToConsole serverLogger


logDebug :: String -> IO ()
logDebug = logDebugToConsole serverLogger
-- |
