module Report.Suffix where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Core as CT
import Report.GroupPath (GroupPath) as S
import Report.Modifiers.Progress (Progress) as S
-- import Report.Modifiers.Stats (Stats) as S
import Report.Modifiers (Modifiers, class IsModifier)
import Report.Modifiers (empty, get, put, keys) as Mod


data Key
    = KProgress
    | KEarnedAt
    | KDescription
    | KReference


derive instance Eq Key
derive instance Ord Key


data Suffix
    = SProgress S.Progress
    | SEarnedAt CT.SDate
    | SDescription String
    | SReference S.GroupPath


type Suffixes = Modifiers Key Suffix


instance IsModifier Key Suffix where
    modifierKey = case _ of
        SProgress _ -> KProgress
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


getProgress :: Suffixes -> Maybe S.Progress
getProgress sfx = get KProgress sfx >>= case _ of
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


getReference :: Suffixes -> Maybe S.GroupPath
getReference sfx = get KReference sfx >>= case _ of
    SReference path -> Just path
    _ -> Nothing