module Data.Array.Extra where

import Prelude

import Data.Array (groupAllBy) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (toArray, head) as NEA

import Data.Tuple.Nested ((/\), type (/\))


groupExt :: forall a b c. Ord b => (a -> b) -> (a -> c) -> Array a -> Array (b /\ Array c)
groupExt = groupExtBy compare


groupExtBy :: forall a b c. (b -> b -> Ordering) -> (a -> b) -> (a -> c) -> Array a -> Array (b /\ Array c)
groupExtBy toOrder getKey toC =
    groupExtByNEA toOrder getKey toC >>> map (map NEA.toArray)


groupExtByNEA :: forall a b c. (b -> b -> Ordering) -> (a -> b) -> (a -> c) -> Array a -> Array (b /\ NonEmptyArray c)
groupExtByNEA toOrder getKey toC =
    Array.groupAllBy (\ia ib -> toOrder (getKey ia) (getKey ib))
    >>> map (\nea -> (getKey $ NEA.head nea) /\ (toC <$> nea))
