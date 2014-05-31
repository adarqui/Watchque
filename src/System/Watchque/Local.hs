{-# LANGUAGE OverloadedStrings #-}

module System.Watchque.Local (
 runLocal
) where

import System.Watchque.Internal (WatchPacket(..), wpktToLocalPath, wpktToLocalArgs, runWatchers)
import System.Watchque.Exception (tryIO)

import Control.Concurrent (forkIO, takeMVar, MVar)
import System.Process (createProcess, waitForProcess, proc)

runLocal :: MVar WatchPacket -> [String] -> IO ()
runLocal mv argv = do
 _ <- forkIO $ localLoop mv (argv !! 0)
 runWatchers mv argv

localLoop :: MVar WatchPacket -> String -> IO ()
localLoop mv loc = do
 wpkt <- takeMVar mv
 res <- tryIO $ createProcess (proc (wpktToLocalPath wpkt loc) (wpktToLocalArgs wpkt))
 case res of
  Left ex -> putStrLn $ show ex
  Right (_,_,_,p) -> waitForProcess p >> return ()
 localLoop mv loc
