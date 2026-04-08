module Report.Decorators.Class.ValueModify where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\))
import Data.String (stripPrefix, Pattern(..)) as String

import Report.Core.Logic (EncodedValue)
import Report.Class
import Report.Chain (Chain)

import Report.Convert.Keyed (keyOf)
import Report.Decorator (Decorator)
import Report.Decorator as Dec
import Report.Decorators.Progress (PValueTag, Progress, valueTagOf)
import Report.Decorators.Progress (PValueTag(..)) as P
import Report.Decorators.Stats (Stats)
import Report.Decorators.Task (TaskP)
import Report.Decorators.Tags (Tags)
import Report.Convert.Text.Decorators.Progress as PEnc
import Report.Convert.Text.Decorators.Task as TEnc
import Report.Convert.Text.Decorators.Stats as SEnc
import Report.Convert.Text.Decorators.Tags as Tags
import Report.Convert.Text.Decorator as DecEnc
import Report.Convert.Keyed (class EncodableKey)


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

instance
    ( ConvertTo (Chain String) t
    , ConvertFrom (Chain String) t
    ) => ValueModify Unit (Tags t) where
    toEditable   = Tags.encodeTags >>> pure
    fromEditable = const $ Just <<< Tags.decodeTags

instance ValueModify Dec.Key Decorator where
    toEditable   = DecEnc.encodeDecorator
    fromEditable = DecEnc.decodeDecorator