{-# LANGUAGE StandaloneDeriving #-}
module System.Watchque.Include (
 Watch(..),
 WatchArg(..),
 WatchEvent,
 WatchPacket(..),
 Handler
) where

import Text.Regex (Regex)
import System.INotify (EventVariety(..))

--deriving instance Show EventVariety

data Watch = Watch {
 _arg :: WatchArg,
 _mask :: [EventVariety],
 _rec :: Bool
}

data WatchArg = WatchArg {
 _class :: String,
 _queue :: String,
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
type Handler = (WatchPacket -> IO ())
