{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
module Config (Config(..), configurate, APIKey, Location) where

import           Data.Aeson
import           GHC.Generics                  ( Generic )
import           System.Directory              ( getCurrentDirectory ) 
import qualified Control.Exception             as E
import qualified Network.HTTP.Client           as HTTPC
import qualified Network.HTTP.Simple           as HTTPS
import qualified Data.Yaml                     as Yaml
import qualified Data.ByteString               as BS

data Config = Config
    { serverPort :: Int
    , apikey :: APIKey
    , rootAPI :: String
    , locations :: [Location]
    -- This option is responsible for the delay between cache fillings 
    , cacheFillingsInterval :: Int -- miliseconds
    -- This option is responsible for the time deviations between each update time of cached weather forecast and query time parameter
    , maxDeviation :: Int -- seconds
    } deriving (Show, Eq, Generic) 

instance FromJSON Config
instance ToJSON Config where
    toEncoding = genericToEncoding defaultOptions

configurate :: IO Config
configurate = do
    cfg <- E.catch
        readConfig 
        (\(e :: E.IOException) -> putStrLn "Config file will be created in the current directory" >> createConfigFile >> readConfig)
    putStrLn "Config was loaded from current directory"
    E.catch
        (checkAuthorization cfg)
        (\(e :: HTTPC.HttpException) -> putStrLn "Autorization failed" >>  changeKey cfg >>= writeToConfigFile >> configurate)

checkAuthorization :: Config -> IO Config
checkAuthorization cfg = do
    HTTPC.parseUrlThrow ("http://api.weatherapi.com/v1/timezone.json?key=" <> apikey cfg <> "&q=London") >>= HTTPS.httpBS
    return cfg

readConfig :: IO Config
readConfig = do
    directory <- getCurrentDirectory
    config <- BS.readFile (directory <> "/config.yaml")
    Yaml.decodeThrow config

createConfigFile :: IO ()
createConfigFile = do
    putStrLn "Default config was created. You can change it in config.yaml"
    config <- changeKey env
    writeToConfigFile config

writeToConfigFile :: Config -> IO ()
writeToConfigFile config = do
    directory <- getCurrentDirectory
    let content = Yaml.encode config
    BS.writeFile (directory <> "/config.yaml") content

changeKey :: Config -> IO Config
changeKey config = do
    putStrLn "Enter the apikey"
    key <- getLine
    return config{apikey = key}

env :: Config
env = Config
    { serverPort = 8080
    , apikey = ""
    , rootAPI = "weatherapi"
    , locations = ["Rostov-on-Don", "Moscow", "Saint Petersburg"]
    , cacheFillingsInterval = 90 * 1000000  -- miliseconds
    , maxDeviation = 3600 -- seconds
    }

type APIKey = String
type Location = String