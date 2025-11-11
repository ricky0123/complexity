module Log where

import Prelude

import Data.DateTime.Instant (Instant, diff)
import Data.Time.Duration (class Duration, Seconds(..))
import Effect (Effect)
import Effect.Now (now)

getTimeElapsedString :: Instant -> Effect String
getTimeElapsedString start = do
  curr <- now
  let
    seconds = diff curr start :: Seconds
  pure $ "time=" <> show seconds

foreign import _debug :: forall a. String -> a -> Effect Unit
