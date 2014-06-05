{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Watchque as WQ (WatchPacket(..), Handler, runArgv)
import qualified System.Watchque.Cli as Cli (dumpWq, dumpPkt)
import System.Environment (getArgs)
import System.DevUtils.Misc (getLineLoop)

main :: IO ()
main = do

 let
  handler :: WQ.Handler
  handler pkt = do
   putStrLn $ Cli.dumpPkt pkt
   return ()

 getArgs >>= \argv -> WQ.runArgv handler argv
 getLineLoop
