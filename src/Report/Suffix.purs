module Report.Suffix where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array

import Report.Core as CT
import Report.GroupPath (GroupPath) as GP

import Report.Modifiers (Modifiers, class IsModifier, modifierKey)
import Report.Modifiers (empty, get, put, keys, toArray) as Mod
import Report.Modifiers.Progress (Progress, PValueTag) as P
import Report.Modifiers.Tags (Tags)


data Key
    = KProgress P.PValueTag
    | KEarnedAt
    | KDescription
    | KReference
    | KTags


derive instance Eq Key
instance Ord Key where
    compare a b = orderIndex a `compare` orderIndex b


data Suffix t
    = SProgress P.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference GP.GroupPath
    | STags (Tags t)


type Suffixes t = Modifiers Key (Suffix t)


instance IsModifier Key (Suffix t) where
    modifierKey = case _ of
        SProgress prog -> KProgress $ modifierKey prog
        SEarnedAt _ -> KEarnedAt
        SDescription _ -> KDescription
        SReference _ -> KReference
        STags _ -> KTags


empty :: forall t. Suffixes t
empty = Mod.empty


keys :: forall t. Suffixes t -> Array Key
keys = Mod.keys


get :: forall t. Key -> Suffixes t -> Maybe (Suffix t)
get = Mod.get


put :: forall t. Suffix t -> Suffixes t -> Suffixes t
put = Mod.put


toArray :: forall t. Suffixes t -> Array (Key /\ Suffix t)
toArray = Mod.toArray


getProgress :: forall t. P.PValueTag -> Suffixes t -> Maybe P.Progress
getProgress pvtag sfx = get (KProgress pvtag) sfx >>= case _ of
    SProgress p -> Just p
    _ -> Nothing


collectProgress :: forall t. Suffixes t -> Array (Maybe P.PValueTag /\ P.Progress)
collectProgress sfx =
    toArray sfx
        <#> (\(key /\ suffix) -> case key /\ suffix of
                KProgress pvtag /\ SProgress p -> Just $ Just pvtag /\ p
                _ /\ SProgress p -> Just $ Nothing /\ p
                _ -> Nothing
            )
         # Array.catMaybes


getEarnedAt :: forall t. Suffixes t -> Maybe CT.SDate
getEarnedAt sfx = get KEarnedAt sfx >>= case _ of
    SEarnedAt d -> Just d
    _ -> Nothing


getDescription :: forall t. Suffixes t -> Maybe String
getDescription sfx = get KDescription sfx >>= case _ of
    SDescription desc -> Just desc
    _ -> Nothing


getReference :: forall t. Suffixes t -> Maybe GP.GroupPath
getReference sfx = get KReference sfx >>= case _ of
    SReference path -> Just path
    _ -> Nothing


getTags :: forall t. Suffixes t -> Maybe (Tags t)
getTags sfx = get KTags sfx >>= case _ of
    STags tags -> Just tags
    _ -> Nothing


orderIndex :: Key -> Int
orderIndex = case _ of
    KProgress _ -> 0
    KEarnedAt -> 1
    KDescription -> 2
    KReference -> 3
    KTags -> 4


debugNavLabel :: Key -> String
debugNavLabel = case _ of
    KProgress pvtag -> "PROG(" <> show pvtag <> ")"
    KEarnedAt -> "EARN"
    KDescription -> "DESC"
    KReference -> "REF"
    KTags -> "TAGS"