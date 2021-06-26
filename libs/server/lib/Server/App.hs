{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module           Server.App ( runApp, configurate) where
-- External modules
import           Network.Wai                   ( Application )
import           GHC.TypeLits                  ( Symbol, someSymbolVal, SomeSymbol(SomeSymbol) )
import           Network.Wai.Handler.Warp      ( run )
import           Servant                       ( Application, Proxy(..), hoistServer, serve, Capture, JSON, type (:>), Get, HasServer(ServerT), Handler )
import           Control.Exception             ( bracket )
import           Control.Monad                 ( forever )
import           Control.Monad.IO.Class        ( liftIO )
import           Control.Monad.Trans.Reader    ( ReaderT, ask, runReaderT )
import           Control.Concurrent            ( forkIO, threadDelay )
import qualified Control.Exception             as E
import qualified Data.Time.Clock.POSIX         as Time
import qualified Data.List                     as L
-- Internal modules
import           Logger                        ( logInfo, logWarn, logDebug )
import           Config                        ( Config(..), configurate )
import           Server.Cache                  ( cacheFiller, readCache, initCacheState, CacheState )
import           Server.ExternalAPI            ( Weather(city, requestTime), getWeatherFromSourceAPI, Location )

-- Run app function
runApp :: Config -> IO ()
runApp enviroment = do
    let port = serverPort enviroment
    cacheState <- initCacheState
    let runServer = logInfo "Server starts" >> run port (app enviroment cacheState)
    let fillCache = case locations enviroment of
            [] -> logWarn "Empty locations field in config. Nothing to cache"
            _  -> logInfo "Cash filler starts" >> cacheFiller enviroment cacheState
    _ <- forkIO fillCache
    _ <- forkIO runServer
    forever $ threadDelay 1800000000

app :: Config -> CacheState -> Application
app env cacheState =
    case someSymbolVal $ rootAPI env of
        SomeSymbol (Proxy :: Proxy var') -> do
            let api = Proxy @(WeatherAPI var')
            serve api (hoistServer api (nt cacheState) (server env cacheState))

type AppM = ReaderT CacheState Handler

server :: Config -> CacheState -> ServerT (WeatherAPI a) AppM
server = getWeather
            -- Query
    where   getWeather :: Config -> CacheState -> Location -> Time.POSIXTime -> AppM Weather
            getWeather env cacheState loc time = do
                cache <- liftIO $ readCache cacheState
                -- Checking the cache for weather forecast
                case L.find (predicate env loc time) cache of
                    Just weather -> liftIO $ logDebug ("" <> show weather <> " found in cash") >> return weather
                    Nothing      -> liftIO $ logDebug "Didn't found anything in cash"          >> getWeatherFromSourceAPI (apikey env) loc

-- This predicate function
-- checks if location name from the query parameter is equal to location name from cash
-- and checks if the time deviation between the request parameter and the data in the cache is within the config parameter 
predicate ::  Config -> Location -> Time.POSIXTime -> (Weather -> Bool)
predicate env loc time = \weather -> 
    (city weather == loc) &&
    abs (time - requestTime weather) <= fromIntegral (maxDeviation env)

nt :: CacheState -> AppM a -> Handler a
nt cacheState appMonad = runReaderT appMonad cacheState

-- GET "localhost:8080/"prefix"/city/*city*/time/*time*
type WeatherAPI (prefix :: Symbol) = prefix :> "city" :> Capture "cityName" String :> "time" :> Capture "epoch" Time.POSIXTime :> Get '[JSON] Weather


