module Report.Modifiers where

import Prelude

import Data.Map (Map)
import Data.Map (empty, lookup, insert, keys, fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe)
import Data.Set (toUnfoldable) as Set
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (class Newtype, unwrap, wrap)

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)

import Report.Convert.Keyed (class Keyed, keyOf)

newtype Modifiers k v = Modifiers (Map k v)
derive instance Newtype (Modifiers k v) _

-- instance (ReadForeign k, ReadForeign v)  => ReadForeign  (Modifiers k v) where readImpl = toArray >>> readImpl
-- instance (WriteForeign k, WriteForeign v) => WriteForeign (Modifiers k v) where writeImpl = toArray >>> writeImpl



empty :: forall k v. Modifiers k v
empty = wrap Map.empty


get :: forall k v. Ord k => k -> Modifiers k v -> Maybe v
get key = unwrap >>> Map.lookup key


keys :: forall k v. Ord k => Modifiers k v -> Array k
keys = unwrap >>> Map.keys >>> Set.toUnfoldable


put :: forall k v. Ord k => Keyed k v => v -> Modifiers k v -> Modifiers k v
put modifier = unwrap >>> Map.insert (keyOf modifier) modifier >>> wrap


toArray :: forall k v. Modifiers k v -> Array (k /\ v)
toArray = unwrap >>> Map.toUnfoldable


fromArray :: forall k v. Ord k => Array (k /\ v) -> Modifiers k v
fromArray = wrap <<< Map.fromFoldable