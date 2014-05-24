{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque (
 Watch(..),
 WatchArg(..),
 WatchEvent(..),
 ss2w,
 s2w,
 s2w',
 s2e,
 e2we,
 chunk,
 wqInit,
 wqAdd,
 wNew,
 wDump,
 toResqueStr
) where

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

wNew :: Watch -> String -> Watch
wNew w f = w { _arg = (_arg w) { _source = f } }

wDump :: Watch -> IO ()
wDump w = do
 putStrLn $ "Dumping watch:" ++ "\n\tsource: " ++ _source aw ++ "\n\tclass: " ++ _class aw ++ "\n\tqueue: " ++ _queue aw ++ "\n\tresque: " ++ _queuePreFormatted aw ++ "\n\tevents: " ++ _events aw ++ "\n\tfilter: " ++ (show (_filter aw)) ++ "\n\tmask: " ++ show (_mask w) ++ "\n\trecursive: " ++ show (_rec w) ++ "\n"
 where
  aw = _arg w

-- --  "{\"class\":\"%s\",\"args\":[{\"filePath\":\"%s/%s\",\"event\":\"%s\"}]}"
toResqueStr :: Watch -> EventVariety -> String -> String -> String
toResqueStr w e isDir f = "{\"class\":\""++(_class $ _arg w)++"\",\"args\":[{\"filePath\":\""++f++",\"event\":\""++(fst we)++"\",\"actual:\""++(snd we)++"\",\"isDir:\""++isDir++"\"}]}"
 where
  we = e2we e

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
   'd' -> [Delete, DeleteSelf]
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
