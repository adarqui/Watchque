module System.Watchque.Exception where

import Control.Exception (try, evaluate, IOException)
import Database.Redis (runRedis, ConnectionLostException, Connection, Redis)
import System.Timeout (timeout)

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

tryRedis :: IO a -> IO (Either ConnectionLostException a)
tryRedis = try

tryRedisBool :: Connection -> Redis a -> IO Bool
tryRedisBool con cb = do
 result <- timeout 5000 . tryRedis . (>>= evaluate) . runRedis con $ do cb
 case result of
  Nothing -> putStrLn "Exception: Timeout" >> return False
  Just _ -> return True
