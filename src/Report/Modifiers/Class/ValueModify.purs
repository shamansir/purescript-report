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
import Report.Convert.Text.Modifiers.Progress as PEnc
import Report.Convert.Text.Modifiers.Task as TEnc
import Report.Convert.Text.Modifiers.Stats as SEnc
import Report.Convert.Text.Modifiers.Tags as Tags
import Report.Convert.Text.Prefix as PxEnc
import Report.Convert.Text.Suffix as SxEnc
import Report.Convert.Tagged (class EncodableKey)

import Report.Prefix as Px
import Report.Suffix as Sx


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