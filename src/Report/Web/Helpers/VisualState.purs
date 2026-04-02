module Report.Web.Helpers.VisualState where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array as Array
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map (lookup, keys, values) as Map
import Data.List (toUnfoldable) as List

import Report.Decorator (Key(..)) as Decorator
import Report.Web.Helpers (H)
import Report.Web.Helpers as H


-- VState is for visually marking the item (using the color of the title, for example), as if it is completed or not and so on
-- this value is extracted from decorators
-- TODO: but we have many types like this one, mostly in `Stats`, may be reuse it?

data ProgressVState
    = Complete
    | Neutral
    | Incomplete
    | Error


derive instance Eq ProgressVState


data VState
    = FromProgress ProgressVState
    | FromRating Number
    | FromPriority Number -- Int
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



itemNameColor :: VState -> H.Color
itemNameColor = case _ of
    FromProgress Complete -> H.completeColor
    FromProgress Incomplete -> H.incompleteColor
    FromProgress Neutral -> H.completeColor
    FromProgress Error -> H.genericColor
    FromPriority _ -> H.completeColor -- FIXME: TODO
    FromRating rating -> H.ratingColor rating
    VNeutral -> H.completeColor