module Report.Web.Navigation where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))

import Report.Core (EncodedValue)
import Report.GroupPath as GP
import Report.Modify (Modification)
import Report.Modify (What(..), WhatKey(..)) as Modify
import Report.Suffix (Key) as Suffix


type NavigatedTo subj_id = -- TODO: add subj_idect, add tabular key
    { mbSubjectId :: Maybe subj_id
    , mbGroup :: Maybe GP.GroupPath
    , mbItem :: Maybe Int
    , mbSuffix :: Maybe Suffix.Key
    , mbEditing :: Maybe { what :: Modify.WhatKey, value :: EncodedValue }
    }


data Location subj_id
    = Nowhere
    | AtGroup  subj_id GP.GroupPath
    | AtItem   subj_id GP.GroupPath Int
    | AtSuffix subj_id GP.GroupPath Int Suffix.Key


init :: forall subj_id. NavigatedTo subj_id
init =
    { mbSubjectId : Nothing
    , mbGroup : Nothing
    , mbItem : Nothing
    , mbSuffix : Nothing
    , mbEditing : Nothing
    }


clear :: forall subj_id. NavigatedTo subj_id
clear = init


toGroup :: forall subj_id. subj_id -> GP.GroupPath -> NavigatedTo subj_id
toGroup subj groupPath =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Nothing
    , mbSuffix : Nothing
    , mbEditing : Nothing
    }


toItem :: forall subj_id. subj_id ->GP.GroupPath -> Int -> NavigatedTo subj_id
toItem subj groupPath itemIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Nothing
    , mbEditing : Nothing
    }


toSuffix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id
toSuffix subj groupPath itemIdx suffixKey =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Just suffixKey
    , mbEditing : Nothing
    }


editGroupName :: forall subj_id. subj_id -> GP.GroupPath -> EncodedValue -> NavigatedTo subj_id
editGroupName subj groupPath encValue =
    toGroup subj groupPath
    # _ { mbEditing = Just { what : Modify.WKGroupName, value : encValue } }


editItemName :: forall subj_id. subj_id -> GP.GroupPath -> Int -> EncodedValue -> NavigatedTo subj_id
editItemName subj groupPath itemIdx encValue =
    toItem subj groupPath itemIdx
    # _ { mbEditing = Just { what : Modify.WKItemName, value : encValue } }


editSuffix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Suffix.Key -> EncodedValue -> NavigatedTo subj_id
editSuffix subj groupPath itemIdx suffixKey encValue =
    toSuffix subj groupPath itemIdx suffixKey
    # _ { mbEditing = Just { what : Modify.WKItemSuffix, value : encValue } }


atGroup :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
atGroup subj groupPath navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
atItem :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atItem subj groupPath itemIdx navigatedTo
    =  atGroup subj groupPath navigatedTo
    && navigatedTo.mbItem == Just itemIdx
atSuffix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id -> Boolean
atSuffix subj groupPath itemIdx suffixKey navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbSuffix == Just suffixKey


editingGroupName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
editingGroupName subj groupPath navigatedTo
    =  atGroup subj groupPath navigatedTo
    && (navigatedTo.mbEditing <#> _.what <#> (_ == Modify.WKGroupName) # fromMaybe false)
editingItemName :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
editingItemName subj groupPath itemIdx navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && (navigatedTo.mbEditing <#> _.what <#> (_ == Modify.WKItemName) # fromMaybe false)
editingAtSuffix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id -> Boolean
editingAtSuffix subj groupPath itemIdx suffixKey navigatedTo
    =  atSuffix subj groupPath itemIdx suffixKey navigatedTo
    && (navigatedTo.mbEditing <#> _.what <#> (_ == Modify.WKItemSuffix) # fromMaybe false)


toModification :: forall subj_id. NavigatedTo subj_id -> Maybe (Modification subj_id)
toModification navigatedTo =
    navigatedTo.mbEditing >>=
        \({ what, value }) ->
            case what of
                Modify.WKGroupName -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : Modify.GroupName
                        , newValue : value
                        }
                Modify.WKItemName -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : Modify.ItemName itemIdx
                        , newValue : value
                        }
                Modify.WKItemSuffix -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    suffixKey <- navigatedTo.mbSuffix
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : Modify.ItemSuffix itemIdx suffixKey
                        , newValue : value
                        }
                Modify.WKItemPrefix -> Nothing -- TODO


toLocation :: forall subj_id. NavigatedTo subj_id -> Location subj_id
toLocation navigatedTo = case (  navigatedTo.mbSubjectId
                              /\ navigatedTo.mbGroup
                              /\ navigatedTo.mbItem
                              /\ navigatedTo.mbSuffix
                              ) of
    ( Nothing /\  _ /\ _ /\ _ ) -> Nowhere
    ( Just subj /\ Just groupPath /\ Nothing /\ _ ) -> AtGroup subj groupPath
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Nothing ) -> AtItem subj groupPath itemIdx
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Just suffixKey ) -> AtSuffix subj groupPath itemIdx suffixKey
    _ -> Nowhere