module Report.Modifiers.Class.ValueModify where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))

import Report.Core.Logic (EncodedValue)
import Report.Class (class IsTag)

import Report.Modifiers (modifierKey)
import Report.Modifiers.Progress (PValueTag, Progress, valueTagOf)
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Task (TaskP)
import Report.Modifiers.Tags (Tags)
import Report.Encoding.Modifiers.Progress as PEnc
import Report.Encoding.Modifiers.Task as TEnc
import Report.Encoding.Modifiers.Stats as SEnc
import Report.Encoding.Modifiers.Tags as Tags
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