module Report.Modifiers where

import Prelude

import Data.Map (Map, lookup)
import Data.Map (empty, lookup, insert, keys) as Map
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable) as Set
import Data.Newtype (class Newtype, unwrap, wrap)


newtype Modifiers k v = Modifiers (Map k v)
derive instance Newtype (Modifiers k v) _


class IsModifier k v where
    modifierKey :: v -> k


empty :: forall k v. Modifiers k v
empty = wrap Map.empty


get :: forall k v. Ord k => k -> Modifiers k v -> Maybe v
get key = unwrap >>> Map.lookup key


keys :: forall k v. Ord k => Modifiers k v -> Array k
keys = unwrap >>> Map.keys >>> Set.toUnfoldable


put :: forall k v. Ord k => IsModifier k v => v -> Modifiers k v -> Modifiers k v
put modifier = unwrap >>> Map.insert (modifierKey modifier) modifier >>> wrap


