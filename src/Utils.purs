module Utils where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Data.Maybe (Maybe(..))

rangeLoop :: forall m a. MonadRec m => Int -> (Int -> Maybe a -> m a) -> m (Maybe a)
rangeLoop n comp = tailRecM go { i: 0, last: Nothing }
  where
  go { i, last }
    | i == n = pure $ Done last
    | otherwise = do
        newValue <- Just <$> comp i last
        pure $ Loop { i: i + 1, last: newValue }

class ToLocaleString a where
  toLocaleString :: a -> String

instance intToLocaleString :: ToLocaleString Int where
  toLocaleString = _intToLocaleString

instance numberToLocaleString :: ToLocaleString Number where
  toLocaleString = _numberToLocaleString

foreign import _intToLocaleString :: Int -> String
foreign import _numberToLocaleString :: Number -> String
