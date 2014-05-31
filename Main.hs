{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Watchque.Run (runMain)

import System.DevUtils.Misc (getLineLoop)

main :: IO ()
main = do
 getArgs >>= runMain
 getLineLoop
