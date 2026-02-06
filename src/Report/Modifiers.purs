module Report.Modifiers where

import Prelude

import Foreign (F, Foreign)

import Data.Map (Map)
import Data.Map (empty, lookup, insert, keys, size, fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Set (toUnfoldable) as Set
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array
import Data.Newtype (class Newtype, unwrap, wrap)

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl, writeImpl)

import Report.Convert.Keyed (class Keyed, keyOf, class EncodableKey, encodeKey, decodeKey)

newtype Modifiers k v = Modifiers (Map k v)
derive instance Newtype (Modifiers k v) _


instance (Ord k, EncodableKey k, ReadForeign v)  => ReadForeign (Modifiers k v)
    where
        readImpl frgn = do
            recArr <- (readImpl frgn :: F (Array { key :: String, value :: v }))
            let tupleArr = recArr <#> (\{ key, value } -> decodeKey @k key <#> \k -> k /\ value)
            pure $ fromArray $ Array.catMaybes tupleArr
instance (EncodableKey k, WriteForeign v) => WriteForeign (Modifiers k v)
    where
        writeImpl modifiers =
            let recArr = toArray modifiers <#> \(k /\ v) -> { key: encodeKey @k k, value: v }
            in  writeImpl recArr



empty :: forall k v. Modifiers k v
empty = wrap Map.empty


size :: forall k v. Modifiers k v -> Int
size = unwrap >>> Map.size


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