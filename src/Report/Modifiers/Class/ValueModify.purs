module Report.Modifiers.Class.ValueModify where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.String (stripPrefix, Pattern(..)) as String

import Report.Core.Logic (EncodedValue)
import Report.Class (class IsTag)

import Report.Modifiers (modifierKey)
import Report.Modifiers.Progress (PValueTag, Progress, valueTagOf)
import Report.Modifiers.Progress (PValueTag(..)) as P
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Task (TaskP)
import Report.Modifiers.Tags (Tags)
import Report.Encoding.Modifiers.Progress as PEnc
import Report.Encoding.Modifiers.Task as TEnc
import Report.Encoding.Modifiers.Stats as SEnc
import Report.Encoding.Modifiers.Tags as Tags
import Report.Encoding.Prefix as PxEnc
import Report.Encoding.Suffix as SxEnc

import Report.Prefix as Px
import Report.Suffix as Sx


class EncodableKey k where
    encodeKey :: k -> String
    decodeKey :: String -> Maybe k


class {- Keyed k a <= -} ValueModify k a where
    toEditable :: a -> k /\ EncodedValue
    fromEditable :: k -> EncodedValue -> Maybe a


instance ValueModify PValueTag Progress where
    toEditable   = PEnc.encodeProgress
    fromEditable = PEnc.decodeProgress

instance ValueModify Unit TaskP where
    toEditable   = TEnc.encodeTask >>> pure
    fromEditable = const $ TEnc.decodeTask

instance ValueModify (Maybe PValueTag) Stats where
    toEditable   = SEnc.encodeStats
    fromEditable = SEnc.decodeStats

instance (IsTag t) => ValueModify Unit (Tags t) where
    toEditable   = Tags.encodeTags >>> pure
    fromEditable = const $ Just <<< Tags.decodeTags

instance (IsTag t) => ValueModify Sx.Key (Sx.Suffix t) where
    toEditable   = SxEnc.encodeSuffix
    fromEditable = SxEnc.decodeSuffix

instance ValueModify Px.Key Px.Prefix where
    toEditable   = PxEnc.encodePrefix
    fromEditable = PxEnc.decodePrefix


instance EncodableKey Px.Key where
    encodeKey = case _ of
        Px.KRating   -> "rating"
        Px.KPriority -> "priority"
        Px.KTask     -> "task"
    decodeKey = case _ of
        "rating"   -> Just Px.KRating
        "priority" -> Just Px.KPriority
        "task"     -> Just Px.KTask
        _          -> Nothing


instance EncodableKey Sx.Key where
    encodeKey = case _ of
        Sx.KProgress (P.PValueTag pvt) -> "progress:" <> pvt
        Sx.KEarnedAt    -> "earnedAt"
        Sx.KDescription -> "description"
        Sx.KReference   -> "reference"
        Sx.KTags        -> "tags"
    decodeKey = case _ of
        "earnedAt"    -> Just Sx.KEarnedAt
        "description" -> Just Sx.KDescription
        "reference"   -> Just Sx.KReference
        "tags"        -> Just Sx.KTags
        str ->           String.stripPrefix (String.Pattern "progress:") str <#> P.PValueTag <#> Sx.KProgress