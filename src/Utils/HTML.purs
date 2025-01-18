module Utils.HTML where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Data.Array (singleton)

_classes = HP.classes <<< singleton <<< HH.ClassName
