{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque (
 Watch(..),
 WatchArg(..),
 ss2w,
 s2w,
 s2w',
 chunk,
 wqInit,
 wqAdd,
 wNew
) where

import System.INotify
import Text.Regex
import Data.Maybe
import Data.List

deriving instance Show EventVariety

data Watch = Watch {
 _arg :: WatchArg,
-- _filterRe :: Maybe Regex,
 _mask :: [EventVariety],
 _rec :: Bool
} deriving (Show)

data WatchArg = WatchArg {
 _class :: String,
 _queue :: String,
 _queuePreFormatted :: String,
 _events :: String,
 _source :: String,
 _filter :: Maybe String
} deriving (Show)

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
   _filter = if length h > 4 then Just (h !! 4) else Nothing
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
   'a' -> [AllEvents]
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

chunk :: Char -> String -> [String]
chunk _ [] = []
chunk d s = h : chunk d (if t == [] then [] else (tail t))
 where
  h = takeWhile (/= d) s
  t = dropWhile (/= d) s
