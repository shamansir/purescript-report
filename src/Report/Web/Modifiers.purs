module Report.Web.Modifiers where

import Data.Maybe (Maybe)

import Report.Core.Logic as CT
import Report.Prefix (Key) as Prefix
import Report.Prefix (Prefixes)
import Report.Suffix (Key) as Suffix
import Report.Suffix (Suffixes)
import Web.UIEvent.MouseEvent (MouseEvent)


type Events i =
    { onEdit :: CT.EncodedValue -> i
    , onEditItemName :: CT.EncodedValue -> i
    , onStartEditing :: MouseEvent -> i
    , onCancelEditing :: i
    , noop :: i
    }


type ModifiersEventConfig k i r =
    { onClick :: k -> MouseEvent -> i
    , onEdit :: k -> CT.EncodedValue -> i
    , onStartEditing :: MouseEvent -> i
    , onCancelEditing :: i
    , noop :: i
    | r
    }


type ModifierEventConfig i r =
    { onEdit :: CT.EncodedValue -> i
    , onClick :: MouseEvent -> i
    , onCancelEditing :: i
    , onStartEditing :: MouseEvent -> i
    , noop :: i
    | r
    }


type PrefixesRenderConfig i =
    ModifiersEventConfig Prefix.Key i
       ( mbSelectedPrefix :: Maybe Prefix.Key
       , isEditingPrefix :: Prefix.Key -> Maybe CT.EncodedValue
       )


type PrefixRenderConfig i =
    ModifierEventConfig i
       ( key :: Prefix.Key
       , isSelected :: Boolean
       , isEditingPrefix :: Maybe CT.EncodedValue
       , parentPrefixes :: Prefixes
       )


type SuffixesRenderConfig i =
    ModifiersEventConfig Suffix.Key i
       ( mbSelectedSuffix :: Maybe Suffix.Key
       , isEditingSuffix :: Suffix.Key -> Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       )


type SuffixRenderConfig i t =
    ModifierEventConfig i
       ( key :: Suffix.Key
       , isSelected :: Boolean
       , isEditingSuffix :: Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       , parentSuffixes :: Suffixes t
       , parentItemName :: String
       )


type ProgressRenderConfig i =
    ModifierEventConfig i
       ( onEditItemName :: CT.EncodedValue -> i
       )