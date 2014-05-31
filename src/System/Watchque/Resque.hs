{-# LANGUAGE OverloadedStrings #-}

module System.Watchque.Resque (
 runResque
) where

import System.Watchque.Internal (wqLaunch, ss2w, WatchPacket(..), wToResqueQueue, wpktToResqueStr, wpktToLocalPath, wpktToLocalArgs, runWatchers)
import System.Watchque.Exception (tryRedisBool)

import Control.Concurrent (forkIO, forkFinally, threadDelay, newEmptyMVar, takeMVar, MVar)
import Control.Exception (try, evaluate, IOException)
import Database.Redis (Connection, defaultConnectInfo, connect, runRedis, ping, rpush, sadd, connectHost, connectPort, PortID (Service), ConnectionLostException, Redis)
import System.Process (createProcess, waitForProcess, proc)
import System.Timeout (timeout)
import qualified Data.ByteString.Char8 as B (pack)

import System.DevUtils.Parser (runUrl)
import qualified System.DevUtils.Data.List as DUL (split)

runResque :: MVar WatchPacket -> [String] -> IO ()
runResque mv argv = do
 runResqueConnector mv rhost rport
 runWatchers mv argv
 where
  argv0 = DUL.split ':' (argv !! 0)
  (rhost,rport) = (argv0 !! 0, argv0 !! 1)

resqueEnqueue :: Connection -> WatchPacket -> IO Bool
resqueEnqueue con wpkt = do
 result <- tryRedisBool con (rpush (B.pack $ wToResqueQueue $ _w $ wpkt) [B.pack $ wpktToResqueStr wpkt])
 case result of
  False -> return False
  True -> return True

resqueHandler :: MVar WatchPacket -> Connection -> IO ()
resqueHandler mv con = do
 wpkt <- takeMVar mv
 result <- resqueEnqueue con wpkt
 case result of
  True -> resqueHandler mv con
  False -> return ()

runResqueConnector :: MVar WatchPacket -> String -> String -> IO ()
runResqueConnector mv rhost rport = do
 _ <- forkFinally (resqueConnector mv rhost rport) (\_ -> threadDelay 1000000 >> runResqueConnector mv rhost rport)
 return ()

resqueConnector :: MVar WatchPacket -> String -> String -> IO ()
resqueConnector mv rhost rport = do
 putStrLn "Attempting to connect."
 result <- timeout 5000 $ connect $ defaultConnectInfo { connectHost = rhost, connectPort = (Service rport) }
 case result of
  Nothing -> return ()
  Just red -> do
   pingResult <- tryRedisBool red ping
   case pingResult of
    True -> putStrLn "Connected" >> resqueHandler mv red
    False -> return ()
 resqueConnector mv rhost rport
