{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad
import System.Watchque
import System.Environment
import System.INotify
import System.Directory
import Database.Redis
import qualified Data.ByteString.Char8 as B
import Data.Maybe

usage :: IO ()
usage = do
 putStrLn "usage: ./watchque [<redishost:port>|</path/to/bin/dir>] <Class1>:<Queue1>:<Events>:<Directory1,...,DirectoryN> ... <ClassN>:<QueueN>:<Events>:<Directory1, ...,DirectoryN>"

getLineLoop :: IO ()
getLineLoop = do
 getLine >> getLineLoop

runArgv :: [String] -> IO ()
runArgv argv = do
 red <- connect $ defaultConnectInfo { connectHost = rhost, connectPort = (Service rport) }
 iN <- wqInit
 let initial_watchers = concat $ ss2w $ tail argv
 mapM_ (\w -> runRecursive red iN w) initial_watchers
 getLineLoop
 return ()
 where
  argv0 = chunk ':' (argv !! 0)
  (rhost,rport) = (argv0 !! 0, argv0 !! 1)

runRecursive :: Connection -> INotify -> Watch -> IO ()
runRecursive red iN w = do
 putStrLn f
 isDir <- doesDirectoryExist f
 case (all (== True) [_rec w, isDir]) of
  True -> do
--   possibleDirs <- getDirectoryContents f
--   putStrLn $ show possibleDirs
--   actualDirs <- filterM (\x -> doesDirectoryExist (f ++ "/" ++ x)) $ filter (\x -> all (/=x) [".", ".."]) possibleDirs
--   putStrLn $ show actualDirs
   actualDirs <- getDirectoryContents f >>= \z -> filterM (\x -> doesDirectoryExist (f ++ "/" ++ x)) $ filter (\x -> all (/=x) [".", ".."]) z
--   mapM_ (\x -> runRecursive red iN (w { _arg = (_arg w) { _source = (f ++ "/" ++ x) }} )) actualDirs
   mapM_ (\x -> runRecursive red iN (wNew w (f ++ "/" ++ x))) actualDirs
   runWatch red iN w
  False -> runWatch red iN w
 return ()
 where
  f = _source $ _arg w

runWatch :: Connection -> INotify -> Watch -> IO ()
runWatch red iN w = do
 putStrLn $ show w
 r <- wqAdd iN w
  (\ev -> do
   let _ = do
        print "fixme"
       wrap e isDir mF cb = do
        putStrLn $ "wrapper" ++ show e ++ show isDir

        if (isDir && _rec w == True && any (==e) [Create,MoveIn])
         then
          do
           putStrLn $ full_path (fromJust mF)
--           runRecursive red iN w { _arg = (_arg w) { _source = full_path (fromJust mF) } }
           runRecursive red iN (wNew w (full_path (fromJust mF)))
         else
          return ()

        if (mF /= Nothing && any (==e) (_mask w))
         then
          do
           _ <- liftIO $ enqueue red (fromJust mF)
           cb
         else
          return ()
       enqueue ev f = do
--  "{\"class\":\"%s\",\"args\":[{\"filePath\":\"%s/%s\",\"event\":\"%s\"}]}"
        runRedis red $ do
         rpush (B.pack (_queuePreFormatted (_arg w))) [B.pack (full_path f)]
       full_path f =
         _source (_arg w) ++ "/" ++ f
       doNothing = do
        return ()
       qOverflow = do
        _ <- liftIO $ enqueue red "overflow"
        putStrLn "qOverflow"
       unknown = do
        print $ "UNKNOWN event" ++ show ev
    in case ev of
     Attributes d f -> wrap Attrib d f $ doNothing
     Created d f -> wrap Create d (Just f) $ doNothing
     Deleted d f -> wrap Delete d (Just f) $ doNothing
     Accessed d f -> wrap Access d f $ doNothing
     Modified d f -> wrap Modify d f $ doNothing
     MovedIn d f _ -> wrap MoveIn d (Just f) $ doNothing
     MovedOut d f _ -> wrap MoveOut d (Just f) $ doNothing
     MovedSelf d -> wrap MoveSelf d (Just "h") $ doNothing
     Opened d f -> wrap Open d f $ doNothing
     Closed d f wW -> wrap (if wW == True then CloseWrite else Close) d f $ doNothing
     QOverflow -> qOverflow
     _  -> unknown
   >>= \x -> print x
   )
 putStrLn $ show r
 return ()

main :: IO ()
main = do
 argv <- getArgs
 case (length argv >= 2) of
  True -> runArgv argv
  False -> usage
