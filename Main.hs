{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, forkFinally, threadDelay, newEmptyMVar, takeMVar, MVar)
import Control.Exception (try, evaluate, IOException)
import System.Watchque (wqLaunch, ss2w, chunk, WatchPacket(..), wToResqueQueue, wpktToResqueStr, wpktToLocalPath, wpktToLocalArgs)
import Database.Redis (Connection, defaultConnectInfo, connect, runRedis, ping, rpush, sadd, connectHost, connectPort, PortID (Service), ConnectionLostException, Redis)
import System.Environment (getArgs)
import System.Process (createProcess, waitForProcess, proc)
import System.Timeout (timeout)
import qualified Data.ByteString.Char8 as B (pack)

import System.DevUtils.Parser (runUrl)

usage :: IO ()
usage = do
 putStrLn "usage: ./watchque [<redishost:port>|</path/to/bin/dir>] <Class1>:<Queue1>:<Events>:<Directory1,...,DirectoryN> ... <ClassN>:<QueueN>:<Events>:<Directory1, ...,DirectoryN>"

getLineLoop :: IO ()
getLineLoop = do
 getLine >> getLineLoop

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

runArgv :: [String] -> IO ()
runArgv argv = do
 mv <- newEmptyMVar
 case (head $ head argv) of
  '/' -> runLocal mv argv
  _ -> runResque mv argv

runWatchers :: MVar WatchPacket -> [String] -> IO ()
runWatchers mv argv = do
 let initial_watchers = concat $ ss2w $ tail argv
 wqLaunch mv initial_watchers

runLocal :: MVar WatchPacket -> [String] -> IO ()
runLocal mv argv = do
 _ <- forkIO $ localLoop mv (argv !! 0)
 runWatchers mv argv
 getLineLoop

runResque :: MVar WatchPacket -> [String] -> IO ()
runResque mv argv = do
 runResqueConnector mv rhost rport
 runWatchers mv argv
 getLineLoop
 where
  argv0 = chunk ':' (argv !! 0)
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

localLoop :: MVar WatchPacket -> String -> IO ()
localLoop mv loc = do
 wpkt <- takeMVar mv
 res <- tryIO $ createProcess (proc (wpktToLocalPath wpkt loc) (wpktToLocalArgs wpkt))
 case res of
  Left ex -> putStrLn $ show ex
  Right (_,_,_,p) -> waitForProcess p >> return ()
 localLoop mv loc

main :: IO ()
main = do
 argv <- getArgs
 case (length argv >= 2) of
  True -> runArgv argv
  False -> usage
