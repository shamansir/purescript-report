module Report.Modifiers.Tags where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)

import Yoga.JSON (class WriteForeign, class ReadForeign)

newtype Tags t = Tags (Array t)
derive instance Newtype (Tags t) _

derive newtype instance WriteForeign t => WriteForeign (Tags t)
derive newtype instance ReadForeign t => ReadForeign (Tags t)


data TagAction
    = SortBy
    | FilterBy
    | GroupBy


derive instance Eq TagAction


toArray :: forall t. Tags t -> Array t
toArray = unwrap


fromArray :: forall t. Array t -> Tags t
fromArray = wrap