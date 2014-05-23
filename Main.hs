{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad
import System.Watchque
import System.Environment
import System.INotify
import Database.Redis
import qualified Data.ByteString.Char8 as B

usage = do
 putStrLn "usage: ./watchque [<redishost:port>|</path/to/bin/dir>] <Class1>:<Queue1>:<Events>:<Directory1,...,DirectoryN> ... <ClassN>:<QueueN>:<Events>:<Directory1, ...,DirectoryN>"

runArgv :: [String] -> IO ()
runArgv argv = do
 red <- connect $ defaultConnectInfo
 iN <- wqInit
 let initial_watchers = concat $ ss2w $ tail argv
 mapM_ (\w -> runWatch red iN w) initial_watchers
 getLine
 return ()
 where
  argv0 = chunk ':' (argv !! 0)
  (rhost,rport) = (argv0 !! 0, argv0 !! 1)

runWatch :: Connection -> INotify -> Watch -> IO ()
runWatch red iN w = do
 wqAdd iN w
  (\ev -> do
   let loop = do
        print "Event received!!" >> print w >> print ev
       enqueue ev f = do
--  "{\"class\":\"%s\",\"args\":[{\"filePath\":\"%s/%s\",\"event\":\"%s\"}]}"
        runRedis red $ do
         rpush (B.pack (_queuePreFormatted (_arg w))) [B.pack (full_path f)]
       full_path f =
         _source (_arg w) ++ "/" ++ f
       created d f = do
        print $ "created" ++ show d ++ show f ++ ((_source $ _arg w) ++ "/" ++ f)
        liftIO $ enqueue red f
        runWatch red iN w { _arg = (_arg w) { _source = (_source (_arg w)) ++ "/" ++ f } }
       deleted d f = do
        print $ "deleted" ++ show d ++ show f
       accessed d f = do
        print $ "accessed" ++ show d ++ show f
       modified d f = do
        print $ "modified" ++ show d ++ show f
       closed d f wW = do
        print $ "closed" ++ show d ++ show f ++ show wW
       movedIn d f ck = do
        print $ "movedIn" ++ show d ++ show f ++ show ck
       movedOut d f ck = do
        print $ "movedOut" ++ show d ++ show f ++ show ck
       movedSelf d = do
        print $ "movedSelf" ++ show d
       qOverflow = do
        print $ "qOverflow"
       unknown = do
        print "UNKNOWN event"
    in case ev of
     Created d f -> created d f
     Deleted d f -> deleted d f
     Accessed d f -> accessed d f
     Modified d f -> accessed d f
     MovedIn d f ck -> movedIn d f ck
     MovedOut d f ck -> movedOut d f ck
     MovedSelf d -> movedSelf d
     Closed d f wW -> closed d f wW
     QOverflow -> qOverflow
     _  -> unknown
   >>= \x -> print x
   )
 return ()

main :: IO ()
main = do
 argv <- getArgs
 case (length argv >= 2) of
  True -> runArgv argv
  False -> usage
 putStrLn "wq"

