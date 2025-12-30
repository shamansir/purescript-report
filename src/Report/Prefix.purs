module Report.Prefix where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Report.Modifiers (Modifiers, class IsModifier)
import Report.Modifiers (empty, get, put, keys, toArray) as Mod
import Report.Modifiers.Priority (Priority)
import Report.Modifiers.Rating (Rating)
import Report.Modifiers.Task (TaskP)


data Key
    = KRating
    | KPriority
    | KTask


derive instance Eq Key
derive instance Ord Key


data Prefix
    = PRating Rating
    | PPriority Priority
    | PTask TaskP


type Prefixes = Modifiers Key Prefix


instance IsModifier Key Prefix where
    modifierKey = case _ of
        PRating _ -> KRating
        PPriority _ -> KPriority
        PTask _ -> KTask


empty :: Prefixes
empty = Mod.empty


keys :: Prefixes -> Array Key
keys = Mod.keys


get :: Key -> Prefixes -> Maybe Prefix
get = Mod.get


put :: Prefix -> Prefixes -> Prefixes
put = Mod.put


toArray :: Prefixes -> Array (Key /\ Prefix)
toArray = Mod.toArray


getRating :: Prefixes -> Maybe Rating
getRating pfx = Mod.get KRating pfx >>= case _ of
    PRating r -> Just r
    _ -> Nothing


getPriotity :: Prefixes -> Maybe Priority
getPriotity pfx = Mod.get KPriority pfx >>= case _ of
    PPriority p -> Just p
    _ -> Nothing


getTask :: Prefixes -> Maybe TaskP
getTask pfx = Mod.get KTask pfx >>= case _ of
    PTask t -> Just t
    _ -> Nothing


debugNavLabel :: Key -> String
debugNavLabel = case _ of
    KRating -> "RATING"
    KPriority -> "PRIORITY"
    KTask -> "TASK"