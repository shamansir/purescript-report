module Report.Modifiers.Tags where

import Prelude

import Data.Newtype (class Newtype)

import Yoga.JSON (class WriteForeign, class ReadForeign)

newtype Tags t = Tags (Array t)
derive instance Newtype (Tags t) _

derive newtype instance WriteForeign t => WriteForeign (Tags t)
derive newtype instance ReadForeign t => ReadForeign (Tags t)