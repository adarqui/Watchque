{-# LANGUAGE OverloadedStrings #-}

module System.Watchque.Run (
 runArgv,
 runMain
) where

import System.Watchque.Local (runLocal)
import System.Watchque.Resque (runResque)

import Control.Concurrent (newEmptyMVar)

usage :: IO ()
usage = do
 putStrLn "usage: ./watchque [<redishost:port>|</path/to/bin/dir>] <Class1>:<Queue1>:<Events>:<Directory1,...,DirectoryN> ... <ClassN>:<QueueN>:<Events>:<Directory1, ...,DirectoryN>"

runArgv :: [String] -> IO ()
runArgv argv = do
 mv <- newEmptyMVar
 case (head $ head argv) of
  '/' -> runLocal mv argv
  _ -> runResque mv argv

runMain :: [String] -> IO ()
runMain argv = do
 case (length argv >= 2) of
  True -> runArgv argv
  False -> usage
