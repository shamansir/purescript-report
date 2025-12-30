module Report.Web.Navigation where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))

import Report.Core.Logic (EncodedValue)
import Report.GroupPath as GP
import Report.Modify (Modification, Location(..))
import Report.Modify (What(..), WhatKey(..), WhatMod(..), WhatModKey(..), loadPrefixKey, loadSuffixKey) as M
import Report.Prefix (Key) as Prefix
import Report.Suffix (Key) as Suffix


type NavigatedTo subj_id = -- TODO: add subj_idect, add tabular key
    { mbSubjectId :: Maybe subj_id
    , mbGroup :: Maybe GP.GroupPath
    , mbItem :: Maybe Int
    , mbModifier :: Maybe M.WhatMod
    , mbEditing :: Maybe { what :: M.WhatKey, value :: EncodedValue }
    }


init :: forall subj_id. NavigatedTo subj_id
init =
    { mbSubjectId : Nothing
    , mbGroup : Nothing
    , mbItem : Nothing
    , mbModifier : Nothing
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
    , mbModifier : Nothing
    , mbEditing : Nothing
    }


toItem :: forall subj_id. subj_id ->GP.GroupPath -> Int -> NavigatedTo subj_id
toItem subj groupPath itemIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbModifier : Nothing
    , mbEditing : Nothing
    }


toSuffix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id
toSuffix subj groupPath itemIdx suffixKey =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbModifier : Just $ M.SuffixMod suffixKey
    , mbEditing : Nothing
    }


toPrefix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Prefix.Key -> NavigatedTo subj_id
toPrefix subj groupPath itemIdx prefixKey =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbModifier : Just $ M.PrefixMod prefixKey
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


editPrefix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Prefix.Key -> EncodedValue -> NavigatedTo subj_id
editPrefix subj groupPath itemIdx prefixKey encValue =
    toPrefix subj groupPath itemIdx prefixKey
    # _ { mbEditing = Just { what : M.WKItemModifier M.WKPrefix, value : encValue } }


editSuffix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Suffix.Key -> EncodedValue -> NavigatedTo subj_id
editSuffix subj groupPath itemIdx suffixKey encValue =
    toSuffix subj groupPath itemIdx suffixKey
    # _ { mbEditing = Just { what : M.WKItemModifier M.WKSuffix, value : encValue } }


atGroup :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
atGroup subj groupPath navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
atItem :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atItem subj groupPath itemIdx navigatedTo
    =  atGroup subj groupPath navigatedTo
    && navigatedTo.mbItem == Just itemIdx
atPrefix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Prefix.Key -> NavigatedTo subj_id -> Boolean
atPrefix subj groupPath itemIdx prefixKey navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbModifier == Just (M.PrefixMod prefixKey)
atSuffix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id -> Boolean
atSuffix subj groupPath itemIdx suffixKey navigatedTo
    =  atItem subj groupPath itemIdx navigatedTo
    && navigatedTo.mbModifier == Just (M.SuffixMod suffixKey)


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
editingAtSuffix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id -> Maybe EncodedValue
editingAtSuffix subj groupPath itemIdx suffixKey navigatedTo
    =  if navigatedTo # atSuffix subj groupPath itemIdx suffixKey
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemModifier M.WKSuffix, value } -> Just value
        _ -> Nothing
    else Nothing
editingAtPrefix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Prefix.Key -> NavigatedTo subj_id -> Maybe EncodedValue
editingAtPrefix subj groupPath itemIdx prefixKey navigatedTo
    =  if navigatedTo # atPrefix subj groupPath itemIdx prefixKey
    then case navigatedTo.mbEditing of
        Just { what : M.WKItemModifier M.WKPrefix, value } -> Just value
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
                M.WKItemModifier M.WKPrefix -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    prefixKey <- navigatedTo.mbModifier >>= M.loadPrefixKey
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemModifier itemIdx $ M.PrefixMod prefixKey
                        , newValue : value
                        }
                M.WKItemModifier M.WKSuffix -> do
                    subjId <- navigatedTo.mbSubjectId
                    groupPath <- navigatedTo.mbGroup
                    suffixKey <- navigatedTo.mbModifier >>= M.loadSuffixKey
                    itemIdx <- navigatedTo.mbItem
                    pure
                        { subjId : subjId
                        , path : groupPath
                        , what : M.ItemModifier itemIdx $ M.SuffixMod suffixKey
                        , newValue : value
                        }



toLocation :: forall subj_id. NavigatedTo subj_id -> Location subj_id
toLocation navigatedTo = case (  navigatedTo.mbSubjectId
                              /\ navigatedTo.mbGroup
                              /\ navigatedTo.mbItem
                              /\ navigatedTo.mbModifier
                              ) of
    ( Nothing /\  _ /\ _ /\ _ ) -> Nowhere
    ( Just subj /\ Just groupPath /\ Nothing /\ _ ) -> AtGroup subj groupPath
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Nothing ) -> AtItem subj groupPath itemIdx
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Just (M.PrefixMod prefixKey) ) -> AtModifier subj groupPath itemIdx $ M.PrefixMod prefixKey
    ( Just subj /\ Just groupPath /\ Just itemIdx /\ Just (M.SuffixMod suffixKey) ) -> AtModifier subj groupPath itemIdx $ M.SuffixMod suffixKey
    _ -> Nowhere