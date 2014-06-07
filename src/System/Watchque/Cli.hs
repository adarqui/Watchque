{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque.Cli (
 dumpWq,
 dumpPkt,
 argv2wqll,
 str2wql,
 str2wq,
 str2evl,
 ev2we
) where

import System.Watchque.Include
 (Watch(..), WatchPacket(..), WatchArg(..), WatchEvent)

import System.INotify
 (EventVariety(..))

import Text.Regex
 (mkRegex)

import Data.List
 (nub)

import qualified System.DevUtils.Base.Data.List as DUL
 (split)

deriving instance Show EventVariety

dumpWq :: Watch -> String
dumpWq wq =
 "Dumping watch:" ++
 "\n\tsource: " ++ _source aw ++
 "\n\tclass: " ++ _class aw ++
 "\n\tqueue: " ++ _queue aw ++
 "\n\tevents: " ++ _events aw ++
 "\n\tfilter: " ++ (show (_filter aw)) ++
 "\n\tmask: " ++ show (_mask wq) ++
 "\n\trecursive: " ++
 show (_rec wq) ++ "\n"
 where
  aw = _arg wq

dumpPkt :: WatchPacket -> String
dumpPkt pkt =
 "Dumping pkt:" ++
 "\n\tpath: " ++ _f pkt ++
 "\n\tevent: " ++ show (_e pkt) ++
 "\n\tdirectory: " ++ show (_d pkt) ++
 "\n\n\t"++(dumpWq $ _w pkt) ++ "\n"

argv2wqll :: [String] -> [[Watch]]
argv2wqll ss = map str2wql ss

str2wql :: String -> [Watch]
str2wql s = map str2wq
 (map
  (\x -> WatchArg {
   _class = h !! 0,
   _queue = h !! 1,
   _events = h !! 2,
   _source = x,
   _filter = if length h > 4 then Just (h !! 4) else Nothing,
   _filterRe = if length h > 4 then Just (mkRegex (h !! 4)) else Nothing
   }
  )
 t)
 where
  h = DUL.split ':' s
  t = DUL.split ',' (h !! 3)

str2wq :: WatchArg -> Watch
str2wq wa = Watch {
 _arg = wa,
 _rec = not $ any (=='N') events,
 _mask = str2evl events
 }
 where
  events = _events wa

str2evl :: String -> [EventVariety]
str2evl [] = []
str2evl (s:ss) = nub $ ev ++ str2evl ss
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

ev2we :: EventVariety -> WatchEvent
ev2we ev = case ev of
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
