module Report.Decorators.Tags where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Array.NonEmpty (NonEmptyArray)

import Yoga.JSON (class WriteForeign, class ReadForeign)

newtype Tags t = Tags (Array t)
derive instance Newtype (Tags t) _

derive newtype instance Functor Tags
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


newtype RawTag = RawTag (NonEmptyArray String)
derive instance Newtype RawTag _
derive newtype instance Eq RawTag
derive newtype instance Ord RawTag
derive newtype instance WriteForeign RawTag
derive newtype instance ReadForeign  RawTag