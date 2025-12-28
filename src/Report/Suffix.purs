module Report.Suffix where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Core as CT
import Report.GroupPath (GroupPath) as GP
import Report.Modifiers.Progress (Progress, PValueTag) as P
-- import Report.Modifiers.Stats (Stats) as S
import Report.Modifiers (Modifiers, class IsModifier, modifierKey)
import Report.Modifiers (empty, get, put, keys) as Mod


data Key
    = KProgress P.PValueTag
    | KEarnedAt
    | KDescription
    | KReference
    -- TODO: make tags suffixes too


derive instance Eq Key
instance Ord Key where
    compare a b = orderIndex a `compare` orderIndex b


data Suffix
    = SProgress P.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference GP.GroupPath


type Suffixes = Modifiers Key Suffix


instance IsModifier Key Suffix where
    modifierKey = case _ of
        SProgress prog -> KProgress $ modifierKey prog
        SEarnedAt _ -> KEarnedAt
        SDescription _ -> KDescription
        SReference _ -> KReference


empty :: Suffixes
empty = Mod.empty


keys :: Suffixes -> Array Key
keys = Mod.keys


get :: Key -> Suffixes -> Maybe Suffix
get = Mod.get


put :: Suffix -> Suffixes -> Suffixes
put = Mod.put


getProgress :: P.PValueTag -> Suffixes -> Maybe P.Progress
getProgress pvtag sfx = get (KProgress pvtag) sfx >>= case _ of
    SProgress p -> Just p
    _ -> Nothing


getEarnedAt :: Suffixes -> Maybe CT.SDate
getEarnedAt sfx = get KEarnedAt sfx >>= case _ of
    SEarnedAt d -> Just d
    _ -> Nothing


getDescription :: Suffixes -> Maybe String
getDescription sfx = get KDescription sfx >>= case _ of
    SDescription desc -> Just desc
    _ -> Nothing


getReference :: Suffixes -> Maybe GP.GroupPath
getReference sfx = get KReference sfx >>= case _ of
    SReference path -> Just path
    _ -> Nothing


orderIndex :: Key -> Int
orderIndex = case _ of
    KProgress _ -> 0
    KEarnedAt -> 1
    KDescription -> 2
    KReference -> 3