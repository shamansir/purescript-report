module Report.Modifiers.Class.ValueModify where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))


import Report.Core (EncodedValue(..))
import Report.Modifiers.Progress
import Report.Modifiers.Encoding.Progress as PEnc
import Report.Modifiers (modifierKey)
import Report.Prefix as Px
import Report.Suffix as Sx



class IsKey k where
    encodeKey :: k -> String
    decodeKey :: String -> Maybe k


class Keyed k a where -- different from `IsModifier`/`modifierKey` key, that key is used to store in the `Map` and this one is for editing
    keyOf :: a -> k


class Keyed k a <= ValueModify k a where
    toEditable :: a -> k /\ EncodedValue
    fromEditable :: k -> EncodedValue -> Maybe a


instance Keyed PValueTag Progress where
    keyOf = valueTagOf

instance Keyed Px.Key Px.Prefix where
    keyOf = modifierKey -- ...but can be used for editing anyway

instance Keyed Sx.Key Sx.Suffix where
    keyOf = modifierKey -- ...but can be used for editing anyway


instance ValueModify PValueTag Progress where
    toEditable = PEnc.encodeProgress
    fromEditable = PEnc.decodeProgress