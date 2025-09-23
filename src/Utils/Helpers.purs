module Utils.Helpers where

import Prelude

import Data.Int as Int
import Data.String as S
import Data.Time.Duration (Seconds(..))

formatElapsedTime :: Seconds -> String
formatElapsedTime (Seconds totalSeconds) = 
  let
    totalSecs = Int.round totalSeconds
    days = totalSecs / 86400
    hours = (totalSecs `mod` 86400) / 3600
    minutes = (totalSecs `mod` 3600) / 60
    seconds = totalSecs `mod` 60
    
    parts = []
      <> (if days > 0 then [show days <> " day" <> (if days == 1 then "" else "s")] else [])
      <> (if hours > 0 then [show hours <> " hr" <> (if hours == 1 then "" else "s")] else [])
      <> (if minutes > 0 then [show minutes <> " min" <> (if minutes == 1 then "" else "s")] else [])
      <> (if seconds > 0 then [show seconds <> " sec" <> (if seconds == 1 then "" else "s")] else [])
  in
    if S.null $ S.joinWith " " parts
      then "0 seconds"
      else S.joinWith " " parts