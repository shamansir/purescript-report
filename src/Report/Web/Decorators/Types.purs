module Report.Web.Decorators.Types where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map (lookup, keys, values) as Map
import Data.List (toUnfoldable) as List

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.Core.Logic as CT
import Report.Decorator (Key(..)) as Decorator
import Report.Decorator (Decorators)
import Report.Decorators.Progress (PValueTag) as Progress
import Report.Web.Helpers (H)
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


type IsComplete = Boolean


type ProgressRenderConfig i =
    EditableValueEvents i
       ( onEditItemName :: CT.EncodedValue -> i
    --    , renderItemName :: IsComplete -> Progress.PValueTag -> H w i
       )




-- VState is for visually marking the item (using the color of the title, for example), as if it is completed or not and so on
-- this value is extracted from decorators
-- TODO: but we have many types like this one, mostly in `Stats`, may be reuse it?

data ProgressVState
    = Incomplete
    | Neutral
    | Complete
    | Error


derive instance Eq ProgressVState


data VState
    = FromRating Number
    | FromPriority Number -- Int
    | FromProgress ProgressVState
    | VNeutral


type VStates = Map Decorator.Key VState


derive instance Eq VState
instance Ord VState where compare = ordVState
instance Semigroup VState where append = mergeVState
instance Monoid VState where mempty = VNeutral


ordVState :: VState -> VState -> Ordering
ordVState (FromProgress _) _ = GT -- progress is more important than priority & rating & neutral
ordVState _ (FromProgress _) = LT -- progress is more important than priority & rating & neutral
ordVState (FromPriority _) _ = GT  -- priority is more important than rating & neutral
ordVState _ (FromPriority _) = LT  -- priority is more important than rating & neutral
ordVState (FromRating _) _ = GT  -- rating is more important than neutral
ordVState _ (FromRating _) = LT -- rating is more important than neutral
ordVState VNeutral VNeutral = EQ


mergeVState :: VState -> VState -> VState
mergeVState (FromProgress progress) _ = FromProgress progress -- progress is more important than priority & rating & neutral
mergeVState _ (FromProgress progress) = FromProgress progress -- progress is more important than priority & rating & neutral
mergeVState (FromPriority priority) _ = FromPriority priority -- priority is more important than rating & neutral
mergeVState _ (FromPriority priority) = FromPriority priority -- priority is more important than rating & neutral
mergeVState (FromRating rating) _ = FromRating rating  -- rating is more important than neutral
mergeVState _ (FromRating rating) = FromRating rating -- rating is more important than neutral
mergeVState VNeutral VNeutral = VNeutral


foldToOne :: Array VState -> VState
foldToOne = fold


selectOne :: VStates -> VState
selectOne vstates =
    case Map.lookup Decorator.KTask vstates of
        Just vstate -> vstate
        Nothing -> Map.values vstates # List.toUnfoldable # foldToOne
