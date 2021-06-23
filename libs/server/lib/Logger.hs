module Logger (logInfo, logWarn, logDebug) where

import qualified Data.Time                     as Time


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