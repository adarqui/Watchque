{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, newEmptyMVar, takeMVar, MVar)
import System.Watchque (wqLaunch, ss2w, chunk, WatchPacket(..), wToResqueQueue, wpktToResqueStr)
import Database.Redis (Connection, defaultConnectInfo, connect, runRedis, rpush, connectHost, connectPort, PortID (Service))
import System.Environment (getArgs)
import qualified Data.ByteString.Char8 as B (pack)

usage :: IO ()
usage = do
 putStrLn "usage: ./watchque [<redishost:port>|</path/to/bin/dir>] <Class1>:<Queue1>:<Events>:<Directory1,...,DirectoryN> ... <ClassN>:<QueueN>:<Events>:<Directory1, ...,DirectoryN>"

getLineLoop :: IO ()
getLineLoop = do
 getLine >> getLineLoop

runArgv :: [String] -> IO ()
runArgv argv = do
 case (head $ head argv) of
  '/' -> runLocal argv
  _ -> runResque argv

runLocal :: [String] -> IO ()
runLocal argv = do
 putStrLn $ show argv

runResque :: [String] -> IO ()
runResque argv = do
 mv <- newEmptyMVar
 _ <- forkIO $ resqueConnector mv rhost rport
 let initial_watchers = concat $ ss2w $ tail argv
 wqLaunch mv initial_watchers
 getLineLoop
 where
  argv0 = chunk ':' (argv !! 0)
  (rhost,rport) = (argv0 !! 0, argv0 !! 1)

resqueEnqueue :: Connection -> WatchPacket -> IO ()
resqueEnqueue con wpkt = do
 _ <- runRedis con $ do
  rpush (B.pack $ wToResqueQueue $ _w $ wpkt) [B.pack $ wpktToResqueStr wpkt]
 return ()

resqueHandler :: MVar WatchPacket -> Connection -> IO ()
resqueHandler mv con = do
 wpkt <- takeMVar mv
 resqueEnqueue con wpkt
 resqueHandler mv con

resqueConnector :: MVar WatchPacket -> String -> String -> IO ()
resqueConnector mv rhost rport = do
 red <- connect $ defaultConnectInfo { connectHost = rhost, connectPort = (Service rport) }
 resqueHandler mv red
 resqueConnector mv rhost rport

main :: IO ()
main = do
 argv <- getArgs
 case (length argv >= 2) of
  True -> runArgv argv
  False -> usage
