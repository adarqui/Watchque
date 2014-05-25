{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque (
 Watch(..),
 WatchArg(..),
 WatchEvent,
 WatchPacket(..),
 ss2w,
 s2w,
 s2w',
 s2e,
 e2we,
 chunk,
 wqInit,
 wqAdd,
 wqLaunch,
 wqRunRecursive,
 wqRunWatch,
 wNew,
 wDump,
 wToResqueQueue,
 wpktNew,
 wpktToResqueStr
) where

import Control.Concurrent (MVar, putMVar)
import Control.Monad (filterM)
import System.Directory
import System.INotify
import Text.Regex
import Data.Maybe
import Data.List

deriving instance Show EventVariety

data Watch = Watch {
 _arg :: WatchArg,
 _mask :: [EventVariety],
 _rec :: Bool
}

data WatchArg = WatchArg {
 _class :: String,
 _queue :: String,
 _queuePreFormatted :: String,
 _events :: String,
 _source :: String,
 _filter :: Maybe String,
 _filterRe :: Maybe Regex
}

data WatchPacket = WatchPacket {
 _w :: Watch,
 _f :: String,
 _e :: EventVariety,
 _d :: Bool
}

type WatchEvent = (String, String)

wqInit :: IO INotify
wqInit = initINotify

wqAdd :: INotify -> Watch -> (Event -> IO ()) -> IO WatchDescriptor
wqAdd iN w cb = do
 addWatch iN mask (_source $ _arg w) cb
 where
  mask = case (_rec w == True) of
   True -> nub $_mask w ++ [Create,MoveIn]
   False -> _mask w

wqLaunch :: MVar WatchPacket -> [Watch] -> IO ()
wqLaunch mv ws = do
 iN <- wqInit
 mapM_ (\w -> wqRunRecursive mv iN w) ws

wqRunRecursive :: MVar WatchPacket -> INotify -> Watch -> IO ()
wqRunRecursive mv iN w = do
 isDir <- doesDirectoryExist f
 case (all (== True) [_rec w, isDir]) of
  True -> do
   actualDirs <- getDirectoryContents f >>= \z -> filterM (\x -> doesDirectoryExist (f ++ "/" ++ x)) $ filter (\x -> all (/=x) [".", ".."]) z
   mapM_ (\x -> wqRunRecursive mv iN (wNew w (f ++ "/" ++ x))) actualDirs
   wqRunWatch mv iN w
  False -> wqRunWatch mv iN w
 where
  f = _source $ _arg w

wqRunWatch :: MVar WatchPacket -> INotify -> Watch -> IO ()
wqRunWatch mv iN w = do
 wDump w
 _ <- wqAdd iN w
  (\ev -> do
   let _ = do
        print "fixme"
       wrap e isDir mF cb = do
        if (isDir && _rec w == True && any (==e) [Create,MoveIn])
         then
          do
           wqRunRecursive mv iN (wNew w (wFullPath w (fromJust mF)))
         else
          return ()

        if (mF /= Nothing && any (==e) (_mask w))
         then
          do
           if (isNothing (_filterRe (_arg w))) || (not (isNothing (matchRegex (fromJust (_filterRe (_arg w))) (wFullPath w (fromJust mF))))) then
            do
             putMVar mv (wpktNew w (fromJust mF) e isDir)
             cb
            else
            return ()
         else
          return ()
       doNothing = do
        return ()
       qOverflow = do
        return ()
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
     MovedSelf d -> wrap MoveSelf d (Just "_") $ doNothing
     Opened d f -> wrap Open d f $ doNothing
     Closed d f wW -> wrap (if wW == True then CloseWrite else Close) d f $ doNothing
     QOverflow -> qOverflow
     _  -> unknown
   >>= \x -> return x
   )
 return ()

wNew :: Watch -> String -> Watch
wNew w f = w { _arg = (_arg w) { _source = f } }

wFullPath :: Watch -> String -> String
wFullPath w f = (_source $ _arg w) ++ "/" ++ f

wDump :: Watch -> IO ()
wDump w = do
 putStrLn $ "Dumping watch:" ++ "\n\tsource: " ++ _source aw ++ "\n\tclass: " ++ _class aw ++ "\n\tqueue: " ++ _queue aw ++ "\n\tresque: " ++ _queuePreFormatted aw ++ "\n\tevents: " ++ _events aw ++ "\n\tfilter: " ++ (show (_filter aw)) ++ "\n\tmask: " ++ show (_mask w) ++ "\n\trecursive: " ++ show (_rec w) ++ "\n"
 where
  aw = _arg w

wpktNew :: Watch -> String -> EventVariety -> Bool -> WatchPacket
wpktNew w f e d = WatchPacket { _w = w, _f = f, _e = e, _d = d }

-- --  "{\"class\":\"%s\",\"args\":[{\"filePath\":\"%s/%s\",\"event\":\"%s\"}]}"
wpktToResqueStr :: WatchPacket -> String
wpktToResqueStr wpkt = "{\"class\":\""++(_class wa)++"\",\"args\":[{\"filePath\":\""++wFullPath w (_f wpkt)++",\"event\":\""++(fst we)++"\",\"actual:\""++(snd we)++"\",\"isDir:\""++(show $ _d wpkt)++"\"}]}"
 where
  w = _w wpkt
  wa = _arg w
  we = e2we $ _e wpkt

wToResqueQueue :: Watch -> String
wToResqueQueue w = "resque:queue:" ++ (_queue $ _arg $ w)

ss2w :: [String] -> [[Watch]]
ss2w ss = map s2w ss

s2w :: String -> [Watch]
s2w s = map s2w'
 (map
  (\x -> WatchArg {
   _class = h !! 0,
   _queue = h !! 1,
   _queuePreFormatted = "resque:queue:"++h!!1,
   _events = h !! 2,
   _source = x,
   _filter = if length h > 4 then Just (h !! 4) else Nothing,
   _filterRe = if length h > 4 then Just (mkRegex (h !! 4)) else Nothing
   }
  )
 t)
 where
  h = chunk ':' s
  t = chunk ',' (h !! 3)

s2w' :: WatchArg -> Watch
s2w' a = Watch {
 _arg = a,
 _rec = not $ any (=='N') events,
 _mask = s2e events
 }
 where
  events = _events a

s2e :: String -> [EventVariety]
s2e [] = []
s2e (s:ss) = nub $ ev ++ s2e ss
 where
  ev = case s of
   'a' -> [AllEvents,Create,MoveIn,Modify,Delete,DeleteSelf,MoveSelf,MoveOut,CloseWrite,CloseNoWrite,Attrib]
   'c' -> [Create, MoveIn]
   'u' -> [Modify, Attrib]
   'd' -> [Delete, DeleteSelf, MoveOut]
   'D' -> [Delete, DeleteSelf]
   'r' -> [MoveSelf, MoveOut]
   'C' -> [CloseWrite]
   'A' -> [Attrib]
   'M' -> [Modify]
   'U' -> [Modify]
   'I' -> [MoveIn]
   'O' -> [MoveOut]
   'Z' -> [Create]
   _ -> []

e2we :: EventVariety -> WatchEvent
e2we e = case e of
 Attrib -> ("MODIFY","ATTRIB")
 Create -> ("CREATE","CREATE")
 Delete -> ("DELETE","DELETE")
 Access -> ("ACCESS","ACCESS")
 Modify -> ("MODIFY","MODIFY")
 MoveIn -> ("CREATE","MOVEIN")
 MoveOut -> ("DELETE","MOVEOUT")
 MoveSelf -> ("DELETE","MOVESELF")
 Open -> ("OPEN","OPEN")
 Close -> ("CLOSE","CLOSE")
 CloseWrite -> ("CLOSE_WRITE","CLOSE")
 CloseNoWrite -> ("CLOSE_NOWRITE","CLOSE")
 _ -> ("UNKNOWN","UNKNOWN")

chunk :: Char -> String -> [String]
chunk _ [] = []
chunk d s = h : chunk d (if t == [] then [] else (tail t))
 where
  h = takeWhile (/= d) s
  t = dropWhile (/= d) s
