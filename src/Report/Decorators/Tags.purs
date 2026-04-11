module Report.Decorators.Tags where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Array.NonEmpty (NonEmptyArray)

import Yoga.JSON (class WriteForeign, class ReadForeign)

import Report.Chain (Chain)

newtype Tags t = Tags (Array t)
derive instance Newtype (Tags t) _

derive newtype instance Functor Tags
derive newtype instance WriteForeign t => WriteForeign (Tags t)
derive newtype instance ReadForeign t => ReadForeign (Tags t)


data TagAction kind tag
    = SortBy kind
    | FilterBy tag
    | GroupBy kind


derive instance (Eq k, Eq t) => Eq (TagAction k t)


toArray :: forall t. Tags t -> Array t
toArray = unwrap


fromArray :: forall t. Array t -> Tags t
fromArray = wrap


empty :: forall t. Tags t
empty = fromArray []


newtype RawTag = RawTag ({ id :: NonEmptyArray String, content :: NonEmptyArray String })
derive instance Newtype RawTag _
derive newtype instance Eq RawTag
derive newtype instance Ord RawTag
derive newtype instance WriteForeign RawTag
derive newtype instance ReadForeign  RawTag


newtype RawTags = RawTags (Array RawTag)
derive instance Newtype RawTags _
derive newtype instance WriteForeign RawTags
derive newtype instance ReadForeign  RawTags