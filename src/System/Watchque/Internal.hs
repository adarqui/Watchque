{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque.Internal (
 init,
 add,
 launch,
 runRecursive,
 runWatch,
 new,
 pktNew,
 runArgv,
 run
) where

import Prelude hiding (init)

import System.Watchque.Include
 (Watch(..), WatchPacket(..), WatchArg(..), Handler)

import System.Watchque.Cli
 (argv2wqll)

import Control.Monad
 (filterM)

import System.Directory
 (doesDirectoryExist, getDirectoryContents)

import System.INotify
 (EventVariety(..), Event(..), INotify, WatchDescriptor, initINotify, addWatch)

import Text.Regex
 (matchRegex)

import Data.Maybe
 (isJust, fromJust, isNothing)

import Data.List
 (nub)

init :: IO INotify
init = initINotify

add :: INotify -> Watch -> (Event -> IO ()) -> IO WatchDescriptor
add iN wq cb = do
 addWatch iN mask (_source $ _arg wq) cb
 where
  mask = case (_rec wq == True) of
   True -> nub $ _mask wq ++ [Create,MoveIn]
   False -> _mask wq

launch :: Handler -> [Watch] -> IO ()
launch f wql = do
 iN <- init
 mapM_ (\wq -> runRecursive f iN wq) wql

runRecursive :: Handler -> INotify -> Watch -> IO ()
runRecursive f iN wq = do
 isDir <- doesDirectoryExist path
 case (all (== True) [_rec wq, isDir]) of
  True -> do
   actualDirs <- getDirectoryContents path >>= \z -> filterM (\x -> doesDirectoryExist (path ++ "/" ++ x)) $ filter (\x -> all (/=x) [".", ".."]) z
   mapM_ (\x -> runRecursive f iN (new wq (path ++ "/" ++ x))) actualDirs
   runWatch f iN wq
  False -> runWatch f iN wq
 where
  path = _source $ _arg wq

runWatch :: Handler -> INotify -> Watch -> IO ()
runWatch f iN wq = do
 _ <- add iN wq
  (\ev -> do
   let _ = do
        print "fixme"
       wrap e isDir mF = do
        if (isDir && _rec wq == True && any (==e) [Create,MoveIn])
         then
          do
           runRecursive f iN (new wq (fullPath wq (fromJust mF)))
         else
          return ()
        if (all (==True) [isJust mF, any (==e) (_mask wq)])
         then
          do
           if (any (==True) [isNothing (_filterRe (_arg wq)), not (isNothing (matchRegex (fromJust (_filterRe (_arg wq))) (fullPath wq (fromJust mF))))]) then
            do
               f $ pktNew wq (fullPath wq (fromJust mF)) e isDir
            else
            return ()
         else
          return ()
       qOverflow = do
        return ()
        putStrLn "qOverflow"
       unknown = do
        print $ "UNKNOWN event" ++ show ev
    in case ev of
     Attributes d p -> wrap Attrib d p
     Created d p -> wrap Create d (Just p)
     Deleted d p -> wrap Delete d (Just p)
     Accessed d p -> wrap Access d p
     Modified d p -> wrap Modify d p
     MovedIn d p _ -> wrap MoveIn d (Just p)
     MovedOut d p _ -> wrap MoveOut d (Just p)
     MovedSelf d -> wrap MoveSelf d (Just "_")
     Opened d p -> wrap Open d p
     Closed d p wW -> wrap (if wW == True then CloseWrite else Close) d p
     QOverflow -> qOverflow
     _  -> unknown
   >>= \x -> return x
   )
 return ()

new :: Watch -> String -> Watch
new wq f = wq { _arg = (_arg wq) { _source = f } }

fullPath :: Watch -> String -> String
fullPath wq f = (_source $ _arg wq) ++ "/" ++ f

pktNew :: Watch -> String -> EventVariety -> Bool -> WatchPacket
pktNew wq f e d = WatchPacket { _w = wq, _f = f, _e = e, _d = d }

runArgv :: Handler -> [String] -> IO ()
runArgv f argv = do
 run f $ concat $ argv2wqll argv
 return ()

run :: Handler -> [Watch] -> IO ()
run f wql = do
 launch f wql
 return ()
