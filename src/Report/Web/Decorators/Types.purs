module Report.Web.Decorators.Types where

import Data.Maybe (Maybe)

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.Core.Logic as CT
import Report.Decorator (Key) as Decorator
import Report.Decorator (Decorators)
{-
import Report.Prefix (Key) as Prefix
import Report.Prefix (Prefixes)
import Report.Suffix (Key) as Suffix
import Report.Suffix (Suffixes)
-}


{-
data TagAction
    = ToFilterBy
    | ToSortBy
    | ToGroupBy
    | ToDelete
-}


type Events i =
    { onEdit :: CT.EncodedValue -> i
    , onEditItemName :: CT.EncodedValue -> i
    , onStartEditing :: MouseEvent -> i
    , onCancelEditing :: i
    , noop :: i
    }


type KeyedEditableValuesEvents k i r =
    { onClick :: k -> MouseEvent -> i
    , onEdit :: k -> CT.EncodedValue -> i
    , onStartEditing :: MouseEvent -> i
    , onCancelEditing :: i
    , noop :: i
    | r
    }


type EditableValueEvents i r =
    { onEdit :: CT.EncodedValue -> i
    , onClick :: MouseEvent -> i
    , onCancelEditing :: i
    , onStartEditing :: MouseEvent -> i
    , noop :: i
    | r
    }


{-
type PrefixesRenderConfig i =
    KeyedEditableValuesEvents Prefix.Key i
       ( mbSelectedPrefix :: Maybe Prefix.Key
       , isEditingPrefix :: Prefix.Key -> Maybe CT.EncodedValue
       )


type PrefixRenderConfig i =
    EditableValueEvents i
       ( key :: Prefix.Key
       , isSelected :: Boolean
       , isEditingPrefix :: Maybe CT.EncodedValue
       , parentPrefixes :: Prefixes
       )


type SuffixesRenderConfig i t =
    KeyedEditableValuesEvents Suffix.Key i
       ( mbSelectedSuffix :: Maybe Suffix.Key
       , isEditingSuffix :: Suffix.Key -> Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       , onTagClick :: t -> MouseEvent -> i
       )


type SuffixRenderConfig i t =
    EditableValueEvents i
       ( key :: Suffix.Key
       , isSelected :: Boolean
       , isEditingSuffix :: Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       , parentSuffixes :: Suffixes t
       , parentItemName :: String
       , onTagClick :: t -> MouseEvent -> i
    --    , onTagMove :: t -> MouseEvent -> TagAction
       )
-}


type DecoratorsRenderConfig i =
    KeyedEditableValuesEvents Decorator.Key i
       ( mbSelectedDecorator :: Maybe Decorator.Key
       , isEditingDecorator :: Decorator.Key -> Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       )


type DecoratorRenderConfig i =
    EditableValueEvents i
       ( key :: Decorator.Key
       , isSelected :: Boolean
       , isEditingDecorator :: Maybe CT.EncodedValue
       , isEditingItemName :: Maybe CT.EncodedValue
       , onEditItemName :: CT.EncodedValue -> i
       , allDecorators :: Decorators
       , parentItemName :: String
    --     key :: Prefix.Key
    --    , isSelected :: Boolean
    --    , isEditingPrefix :: Maybe CT.EncodedValue
    --    , parentPrefixes :: Prefixes
       )


type TagsRenderConfig i t =
    EditableValueEvents i
        ( isSelected :: Boolean
        , isEditingTags :: Maybe CT.EncodedValue
        , onTagClick :: t -> MouseEvent -> i
    --    , onTagMove :: t -> MouseEvent -> TagAction
        )


type ProgressRenderConfig i =
    EditableValueEvents i
       ( onEditItemName :: CT.EncodedValue -> i
       )