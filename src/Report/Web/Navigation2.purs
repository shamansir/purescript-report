module Report.Web.Navigation2 where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))

import Report.Core.Logic (EncodedValue)
import Report.GroupPath as GP
import Report.Modify (Modification, Location(..))
import Report.Modify (What(..), WhatKey(..), whatKeyOfLoc) as M
import Report.Decorator (Key) as Decorator


data NavigatedTo subj_id
    = At (Location subj_id)
    | Editing (Location subj_id) EncodedValue


derive instance Eq subj_id => Eq (NavigatedTo subj_id)


type NavigatedToRec subj_id = -- TODO: add tabular key
    { mbSubjectId :: Maybe subj_id
    , mbGroup :: Maybe GP.GroupPath
    , mbItem :: Maybe Int
    , mbDecorator :: Maybe Decorator.Key
    , mbTag :: Maybe Int
    , mbTabular :: Maybe Int
    , mbEditing :: Maybe { what :: M.WhatKey, value :: EncodedValue }
    }



-- FIXME: store the Location directly in NavigatedTo instead of computing it on the fly;
--        on the other hand, though, it easier to just set `Maybe`s from the UI itself
--        and then manage the consistency / inconsistency in one place here;
--        `mbEditing` also looks like a complication, since it also stores a `WhatKey` as
--        the location...
--        `mbEditing` can be just `EncodedValue` then...


init :: forall subj_id. NavigatedTo subj_id
init = At Nowhere


initRec :: forall subj_id. NavigatedToRec subj_id
initRec =
    { mbSubjectId : Nothing
    , mbGroup : Nothing
    , mbItem : Nothing
    , mbDecorator : Nothing
    , mbTag : Nothing
    , mbTabular : Nothing
    , mbEditing : Nothing
    }


clear :: forall subj_id. NavigatedTo subj_id
clear = init


mkEditing :: forall subj_id. EncodedValue -> NavigatedTo subj_id -> NavigatedTo subj_id
mkEditing encValue = case _ of
    At location -> Editing location encValue
    Editing location _ -> Editing location encValue


isEditing :: forall subj_id. NavigatedTo subj_id -> Boolean
isEditing = case _ of
    At _ -> false
    Editing _ _ -> true


mbEditing :: forall subj_id. NavigatedTo subj_id -> Maybe EncodedValue
mbEditing = case _ of
    At _ -> Nothing
    Editing _ encValue -> Just encValue


clearEditing :: forall subj_id. NavigatedTo subj_id -> NavigatedTo subj_id
clearEditing = case _ of
    At location -> At location
    Editing location _ -> At location


toGroup :: forall subj_id. subj_id -> GP.GroupPath -> NavigatedTo subj_id
toGroup subj groupPath =
    At $ AtGroup subj groupPath


toItem :: forall subj_id. subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id
toItem subj groupPath itemIdx =
    At $ AtItem subj groupPath itemIdx


toDecorator :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id
toDecorator subj groupPath itemIdx decoratorKey =
    At $ AtDecorator subj groupPath itemIdx decoratorKey


toTag :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id
toTag subj groupPath itemIdx tagIdx =
    At $ AtTag subj groupPath itemIdx tagIdx


toTabular :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id
toTabular subj groupPath itemIdx tabularIdx =
    At $ AtTabular subj groupPath itemIdx tabularIdx


editGroupName :: forall subj_id. subj_id -> GP.GroupPath -> EncodedValue -> NavigatedTo subj_id
editGroupName subj groupPath encValue =
    toGroup subj groupPath
    # mkEditing encValue


editItemName :: forall subj_id. subj_id -> GP.GroupPath -> Int -> EncodedValue -> NavigatedTo subj_id
editItemName subj groupPath itemIdx encValue =
    toItem subj groupPath itemIdx
    # mkEditing encValue


editDecorator :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Decorator.Key -> EncodedValue -> NavigatedTo subj_id
editDecorator subj groupPath itemIdx decoratorKey encValue =
    toDecorator subj groupPath itemIdx decoratorKey
    # mkEditing encValue


editTag :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> EncodedValue -> NavigatedTo subj_id
editTag subj groupPath itemIdx tagIdx encValue =
    toTag subj groupPath itemIdx tagIdx
    # mkEditing encValue


editTabular :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> EncodedValue -> NavigatedTo subj_id
editTabular subj groupPath itemIdx tabularIdx encValue =
    toTabular subj groupPath itemIdx tabularIdx
    # mkEditing encValue


_toNavigationRec :: forall subj_id. NavigatedTo subj_id -> NavigatedToRec subj_id
_toNavigationRec = case _ of
    At location ->
        fillLocation location
    Editing location encValue ->
        fillLocation location # _ { mbEditing = M.whatKeyOfLoc location <#> \whk -> { what : whk, value : encValue } }
    where
        fillLocation = case _ of
            Nowhere -> initRec
            AtGroup subj groupPath ->
                initRec # _ { mbSubjectId = Just subj, mbGroup = Just groupPath }
            AtItem subj groupPath itemIdx ->
                initRec # _ { mbSubjectId = Just subj, mbGroup = Just groupPath, mbItem = Just itemIdx }
            AtDecorator subj groupPath itemIdx decoratorKey ->
                initRec # _ { mbSubjectId = Just subj, mbGroup = Just groupPath, mbItem = Just itemIdx, mbDecorator = Just decoratorKey }
            AtTag subj groupPath itemIdx tagIdx ->
                initRec # _ { mbSubjectId = Just subj, mbGroup = Just groupPath, mbItem = Just itemIdx, mbTag = Just tagIdx }
            AtTabular subj groupPath itemIdx tabularIdx ->
                initRec # _ { mbSubjectId = Just subj, mbGroup = Just groupPath, mbItem = Just itemIdx, mbTabular = Just tabularIdx }


atGroupR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedToRec subj_id -> Boolean
atGroupR subj groupPath = \navigatedTo
    -> navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
atItemR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedToRec subj_id -> Boolean
atItemR subj groupPath itemIdx = \navigatedTo
    -> atGroupR subj groupPath navigatedTo
    && navigatedTo.mbItem == Just itemIdx
atDecoratorR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedToRec subj_id -> Boolean
atDecoratorR subj groupPath itemIdx decoratorKey = \navigatedTo
    -> atItemR subj groupPath itemIdx navigatedTo
    && navigatedTo.mbDecorator == Just decoratorKey
atTagsR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedToRec subj_id -> Boolean
atTagsR subj groupPath itemIdx = \navigatedTo
    -> atItemR subj groupPath itemIdx navigatedTo
    && isJust navigatedTo.mbTag
atTagR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedToRec subj_id -> Boolean
atTagR subj groupPath itemIdx tagIdx = \navigatedTo
    -> atItemR subj groupPath itemIdx navigatedTo
    && navigatedTo.mbTag == Just tagIdx
atTabularR :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedToRec subj_id -> Boolean
atTabularR subj groupPath itemIdx tabularIdx = \navigatedTo
    -> atItemR subj groupPath itemIdx navigatedTo
    && navigatedTo.mbTabular == Just tabularIdx


atGroup :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
atGroup subj groupPath = _toNavigationRec >>> atGroupR subj groupPath
atItem :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atItem subj groupPath itemIdx = _toNavigationRec >>> atItemR subj groupPath itemIdx
atDecorator :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id -> Boolean
atDecorator subj groupPath itemIdx decoratorKey = _toNavigationRec >>> atDecoratorR subj groupPath itemIdx decoratorKey
atTags :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atTags subj groupPath itemIdx = _toNavigationRec >>> atTagsR subj groupPath itemIdx
atTag :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id -> Boolean
atTag subj groupPath itemIdx tagIdx = _toNavigationRec >>> atTagR subj groupPath itemIdx tagIdx
atTabular :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id -> Boolean
atTabular subj groupPath itemIdx tabularIdx = _toNavigationRec >>> atTabularR subj groupPath itemIdx tabularIdx



editingGroupName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Maybe EncodedValue
editingGroupName subj groupPath = _toNavigationRec >>> \navigatedTo
    -> if navigatedTo # atGroupR subj groupPath
    then case navigatedTo.mbEditing of
        Just { what : M.WKGroupName, value } -> Just value
        _ -> Nothing
    else Nothing
editingItemName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Maybe EncodedValue
editingItemName subj groupPath itemIdx = _toNavigationRec >>> \navigatedTo
    -> if navigatedTo # atItemR subj groupPath itemIdx
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemName, value } -> Just value
        _ -> Nothing
    else Nothing
editingAtDecorator :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id -> Maybe EncodedValue
editingAtDecorator subj groupPath itemIdx decoratorKey = _toNavigationRec >>> \navigatedTo
    -> if navigatedTo # atDecoratorR subj groupPath itemIdx decoratorKey
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemDecorator, value } -> Just value
        _ -> Nothing
    else Nothing


toModification :: forall subj_id. NavigatedTo subj_id -> Maybe (Modification subj_id)
toModification = _toNavigationRec >>> \navigatedTo ->
    navigatedTo.mbEditing >>=
        \({ what, value }) ->
            case what of
                M.WKGroupName -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.GroupName
                        , newValue : value
                        }
                M.WKItemName -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemName itemIdx
                        , newValue : value
                        }
                M.WKItemDecorator -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    decoratorKey <- navigatedTo.mbDecorator
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemDecorator itemIdx decoratorKey
                        , newValue : value
                        }
                M.WKItemTags -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    itemIdx <- navigatedTo.mbItem
                    tagIdx <- navigatedTo.mbTag
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemTag itemIdx tagIdx
                        , newValue : value
                        }
                M.WKItemTabular -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    itemIdx <- navigatedTo.mbItem
                    tabularIdx <- navigatedTo.mbTabular
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemTabular itemIdx tabularIdx
                        , newValue : value
                        }


toLocation :: forall subj_id. NavigatedTo subj_id -> Location subj_id
toLocation = case _ of
    At location -> location
    Editing location _ -> location