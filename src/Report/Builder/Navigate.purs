module Report.Builder.Navigate where


import Prelude

import Data.Maybe
import Data.Newtype
import Data.Array as Array

import Report.Builder
import Report.Builder as Builder
import Report.Class as R
import Report.GroupPath
import Report.Decorator as Decorator


nextSubject
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => subj_id
    -> Builder subj group item
    -> Maybe subj_id
nextSubject subjId builder =
    let
        subjects = Builder.allSubjects builder
    in
        subjects
             # Array.findIndex (\subj -> R.s_id subj == subjId)
            <#> (_ + 1)
            >>= Array.index subjects
            <#> R.s_id


previousSubject
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => subj_id
    -> Builder subj group item
    -> Maybe subj_id
previousSubject subjId builder =
    let
        subjects = Builder.allSubjects builder
    in
        subjects
             # Array.findIndex (\subj -> R.s_id subj == subjId)
            <#> (_ - 1)
            >>= Array.index subjects
            <#> R.s_id


nextGroup
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Builder subj group item
    -> Maybe GroupPath
nextGroup subjId groupPath builder =
    let
        groups = Builder.allGroupsOf subjId builder
    in
        groups
             # Array.findIndex (\group -> R.g_path group == groupPath)
            <#> (_ + 1)
            >>= Array.index groups
            <#> R.g_path


previousGroup
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Builder subj group item
    -> Maybe GroupPath
previousGroup subjId groupPath builder =
    let
        groups = Builder.allGroupsOf subjId builder
    in
        groups
             # Array.findIndex (\group -> R.g_path group == groupPath)
            <#> (_ - 1)
            >>= Array.index groups
            <#> R.g_path


nextItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Int
    -> Builder subj group item
    -> Maybe Int
nextItem subjId groupPath itemIdx builder =
    let
        itemsOfGroup = fromMaybe [] $ Builder.directItemsOf subjId groupPath builder
        itemsCount = Array.length itemsOfGroup
        nextIdx = itemIdx + 1
    in
        if nextIdx < itemsCount && nextIdx >= 0 then Just nextIdx else Nothing


previousItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Int
    -> Builder subj group item
    -> Maybe Int
previousItem subjId groupPath itemIdx builder =
    let
        itemsOfGroup = fromMaybe [] $ Builder.directItemsOf subjId groupPath builder
        itemsCount = Array.length itemsOfGroup
        prevIdx = itemIdx - 1
    in
        if prevIdx < itemsCount && prevIdx >= 0 then Just prevIdx else Nothing


{-
nextDecorator :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Decorator.Key -> Builder subj group item -> Maybe Decorator.Key
nextDecorator = ?wg


previousDecorator :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Decorator.Key -> Builder subj group item -> Maybe Decorator.Key
previousDecorator = ?wg


nextTag :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Int -> Builder subj group item -> Maybe Int
nextTag = ?wg


previousTag :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Int -> Builder subj group item -> Maybe Int
previousTag = ?wg


nextTabular :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Int -> Builder subj group item -> Maybe Int
nextTabular = ?wg


previousTabular :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Int -> Builder subj group item -> Maybe Int
previousTabular = ?wg


firstGroupInSubj :: forall subj_id subj group item. subj_id -> Builder subj group item -> Maybe GroupPath
firstGroupInSubj = ?wg


lastGroupInSubj :: forall subj_id subj group item. subj_id -> Builder subj group item -> Maybe GroupPath
lastGroupInSubj = ?wg


firstItemInGroup :: forall subj_id subj group item. subj_id -> GroupPath -> Builder subj group item -> Maybe Int
firstItemInGroup = ?wg


lastItemInGroup :: forall subj_id subj group item. subj_id -> GroupPath -> Builder subj group item -> Maybe Int
lastItemInGroup = ?wg


firstDecoratorInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Decorator.Key
firstDecoratorInItem = ?wg


lastDecoratorInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Decorator.Key
lastDecoratorInItem = ?wg


firstTabularInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Int
firstTabularInItem = ?wg


lastTabularInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Int
lastTabularInItem = ?wg
-}