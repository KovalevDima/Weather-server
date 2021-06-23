module           Server.Cache ( cacheFiller, CacheState, initCacheState, readCache) where
import           Control.Concurrent.STM.TVar   ( TVar, newTVar, readTVar, writeTVar, readTVarIO, newTVarIO )
import           Control.Monad.STM             ( atomically )
import           Control.Concurrent            ( threadDelay )

import           Config                        ( Config(apikey, locations, cacheFillingsInterval) )
import           Logger                        ( logInfo, logDebug )
import           Server.ExternalAPI                   ( Weather, getWeatherFromSourceAPI )
            
newtype CacheState = CacheState
    { weatherCache :: TVar [Weather]
    }

cacheFiller :: Config -> CacheState -> IO ()
cacheFiller enviroment cacheState = do
    -- Here we send requests for all cities specified in the config
    weather <- mapM (getWeatherFromSourceAPI (apikey enviroment)) (locations enviroment)
    -- and then write forecasts to the cash
    writeToCache cacheState weather
    logInfo $ show (length weather) <> " weather forecasts added to cache"
    cash <- readCache cacheState
    logDebug $ "Current cash state: " <> show cash
    threadDelay $ cacheFillingsInterval enviroment -- Delay between cacheFiller calls
    cacheFiller enviroment cacheState

readCache :: CacheState -> IO [Weather]
readCache cacheState = do
    let CacheState{weatherCache = p} = cacheState
    readTVarIO p

writeToCache :: CacheState -> [Weather] -> IO ()
writeToCache cacheState forecastsList = do
    let CacheState{weatherCache = tvar} = cacheState
    atomically $ readTVar tvar >>= writeTVar tvar . (forecastsList ++)

initCacheState :: IO CacheState
initCacheState = CacheState <$> newTVarIO []