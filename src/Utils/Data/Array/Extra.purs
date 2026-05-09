module Data.Array.Extra where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array (elemIndex, groupAllBy, index, length)  as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray, head) as NEA

import Data.Tuple.Nested ((/\), type (/\))


next :: forall a. Int -> Array a -> Maybe a
next idx arr =
    nextIndex idx arr >>= Array.index arr


nextTo :: forall a. Eq a => a -> Array a -> Maybe a
nextTo a arr =
    Array.elemIndex a arr >>= flip next arr


nextIndex :: forall a. Int -> Array a -> Maybe Int
nextIndex idx arr =
    let nextIdx = idx + 1 in if nextIdx >= 0 && nextIdx < Array.length arr then Just nextIdx else Nothing


prev :: forall a. Int -> Array a -> Maybe a
prev idx arr =
    prevIndex idx arr >>= Array.index arr


prevTo :: forall a. Eq a => a -> Array a -> Maybe a
prevTo a arr =
    Array.elemIndex a arr >>= flip prev arr


prevIndex :: forall a. Int -> Array a -> Maybe Int
prevIndex idx arr =
    let prevIdx = idx - 1 in if prevIdx >= 0 && prevIdx < Array.length arr then Just prevIdx else Nothing


groupExt :: forall a b c. Ord b => (a -> b) -> (a -> c) -> Array a -> Array (b /\ Array c)
groupExt = groupExtBy compare


groupExtBy :: forall a b c. (b -> b -> Ordering) -> (a -> b) -> (a -> c) -> Array a -> Array (b /\ Array c)
groupExtBy toOrder getKey toC =
    groupExtByNEA toOrder getKey toC >>> map (map NEA.toArray)


groupExtByNEA :: forall a b c. (b -> b -> Ordering) -> (a -> b) -> (a -> c) -> Array a -> Array (b /\ NonEmptyArray c)
groupExtByNEA toOrder getKey toC =
    Array.groupAllBy (\ia ib -> toOrder (getKey ia) (getKey ib))
    >>> map (\nea -> (getKey $ NEA.head nea) /\ (toC <$> nea))
