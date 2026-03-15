module Report.Web.Navigation where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))

import Report.Core.Logic (EncodedValue)
import Report.GroupPath as GP
import Report.Modify (Modification, Location(..))
import Report.Modify (What(..), WhatKey(..)) as M
import Report.Decorator (Key) as Decorator


type NavigatedTo subj_id = -- TODO: add subj_idect, add tabular key
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
init =
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


isEditing :: forall subj_id. NavigatedTo subj_id -> Boolean
isEditing = _.mbEditing >>> isJust


clearEditing :: forall subj_id. NavigatedTo subj_id -> NavigatedTo subj_id
clearEditing = _ { mbEditing = Nothing }


toGroup :: forall subj_id. subj_id -> GP.GroupPath -> NavigatedTo subj_id
toGroup subj groupPath =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Nothing
    , mbDecorator : Nothing
    , mbTag : Nothing
    , mbTabular : Nothing
    , mbEditing : Nothing
    }


toItem :: forall subj_id. subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id
toItem subj groupPath itemIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbDecorator : Nothing
    , mbTag : Nothing
    , mbTabular : Nothing
    , mbEditing : Nothing
    }


toDecorator :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id
toDecorator subj groupPath itemIdx decoratorKey =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbDecorator : Just decoratorKey
    , mbTag : Nothing
    , mbTabular : Nothing
    , mbEditing : Nothing
    }


toTag :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id
toTag subj groupPath itemIdx tagIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbDecorator : Nothing
    , mbTag : Just tagIdx
    , mbTabular : Nothing
    , mbEditing : Nothing
    }


toTabular :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id
toTabular subj groupPath itemIdx tabularIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbDecorator : Nothing
    , mbTag : Nothing
    , mbTabular : Just tabularIdx
    , mbEditing : Nothing
    }


editGroupName :: forall subj_id. subj_id -> GP.GroupPath -> EncodedValue -> NavigatedTo subj_id
editGroupName subj groupPath encValue =
    toGroup subj groupPath
    # _ { mbEditing = Just { what : M.WKGroupName, value : encValue } }


editItemName :: forall subj_id. subj_id -> GP.GroupPath -> Int -> EncodedValue -> NavigatedTo subj_id
editItemName subj groupPath itemIdx encValue =
    toItem subj groupPath itemIdx
    # _ { mbEditing = Just { what : M.WKItemName, value : encValue } }


editDecorator :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Decorator.Key -> EncodedValue -> NavigatedTo subj_id
editDecorator subj groupPath itemIdx decoratorKey encValue =
    toDecorator subj groupPath itemIdx decoratorKey
    # _ { mbEditing = Just { what : M.WKItemDecorator, value : encValue } }


atGroup :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
atGroup subj groupPath navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
atItem :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atItem subj groupPath itemIdx navigatedTo
    =  atGroup subj groupPath navigatedTo
    && navigatedTo.mbItem == Just itemIdx
atDecorator :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id -> Boolean
atDecorator subj groupPath itemIdx decoratorKey navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbDecorator == Just decoratorKey
atTags :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atTags subj groupPath itemIdx navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && isJust navigatedTo.mbTag
atTag :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id -> Boolean
atTag subj groupPath itemIdx tagIdx navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbTag == Just tagIdx
atTabular :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Int -> NavigatedTo subj_id -> Boolean
atTabular subj groupPath itemIdx tabularIdx navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbTabular == Just tabularIdx



editingGroupName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Maybe EncodedValue
editingGroupName subj groupPath navigatedTo
    =  if navigatedTo # atGroup subj groupPath
    then case navigatedTo.mbEditing of
        Just { what : M.WKGroupName, value } -> Just value
        _ -> Nothing
    else Nothing
editingItemName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Maybe EncodedValue
editingItemName subj groupPath itemIdx navigatedTo
    =  if navigatedTo # atItem subj groupPath itemIdx
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemName, value } -> Just value
        _ -> Nothing
    else Nothing
editingAtDecorator :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Decorator.Key -> NavigatedTo subj_id -> Maybe EncodedValue
editingAtDecorator subj groupPath itemIdx decoratorKey navigatedTo
    =  if navigatedTo # atDecorator subj groupPath itemIdx decoratorKey
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemDecorator, value } -> Just value
        _ -> Nothing
    else Nothing


toModification :: forall subj_id. NavigatedTo subj_id -> Maybe (Modification subj_id)
toModification navigatedTo =
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
                        , what : M.ItemTags itemIdx tagIdx
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


-- FIXME: store the Location directly in NavigatedTo instead of computing it on the fly
--        on the other hand, though, it easier to just set `Maybe`s from the UI itself
--        and then manage the consistency / inconsistency in one place here
toLocation :: forall subj_id. NavigatedTo subj_id -> Location subj_id
toLocation navigatedTo = case (  navigatedTo.mbSubjectId
                              /\ navigatedTo.mbGroup
                              /\ navigatedTo.mbItem
                              /\ navigatedTo.mbDecorator
                              /\ navigatedTo.mbTag
                              /\ navigatedTo.mbTabular
                              ) of
    ( Nothing /\ _ /\ _ /\ _ /\ _ /\ _ ) ->                                                       Nowhere
    ( Just subj /\ Just groupPath /\ Nothing /\ _ /\ _ /\ _ ) ->                                  AtGroup subj groupPath
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Nothing /\ Nothing /\ Nothing ) ->           AtItem subj groupPath itemIdx
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Just decoratorKey /\ Nothing /\ Nothing ) -> AtDecorator subj groupPath itemIdx decoratorKey
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ _ /\ Just tagIdx /\ Nothing ) ->             AtTag subj groupPath itemIdx tagIdx
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ _ /\ _ /\ Just tabularIdx ) ->               AtTabular subj groupPath itemIdx tabularIdx
    _ -> Nowhere