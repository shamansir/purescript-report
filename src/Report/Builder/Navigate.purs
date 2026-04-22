module Report.Builder.Navigate where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (findIndex, head, index, insert, last, length)  as Array
import Data.Array.Extra (next, nextIndex, nextTo, prev, prevIndex, prevTo) as Array

import Report.Builder (Builder)
import Report.Builder as Builder
import Report.Class as R
import Report.GroupPath (GroupPath)
import Report.Decorator as Decorator
-- import Report.Decorator (Key(..)) as D
import Report.Tabular (items) as Tab
import Report.Builder.InlinePos (PosKey(..), toPosKey)


type ItemIndex = Int
type TabularIndex = Int


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
            >>= flip Array.next subjects
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
            >>= flip Array.prev subjects
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
            >>= flip Array.next groups
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
            >>= flip Array.prev groups
            <#> R.g_path


nextItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> Builder subj group item
    -> Maybe ItemIndex
nextItem subjId groupPath itemIdx builder =
    Array.nextIndex itemIdx $ fromMaybe [] $ Builder.directItemsOf subjId groupPath builder


previousItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> Builder subj group item
    -> Maybe ItemIndex
previousItem subjId groupPath itemIdx builder =
    Array.prevIndex itemIdx $ fromMaybe [] $ Builder.directItemsOf subjId groupPath builder


_itemMap
    :: forall @item_tag item
     . R.HasDecorators item
    => R.HasTags item_tag item
    => item
    -> Array PosKey
_itemMap item =
    let
        decorators = R.i_decorators item
        decoratorKeys = Decorator.keys decorators
        tags = R.i_tags @item_tag item
    in if Array.length tags > 0 then
        {- Array.nub $ -} Array.insert PKItemName $ Array.insert PKTags $ toPosKey <$> decoratorKeys
    else {- Array.nub $ -} Array.insert PKItemName $ toPosKey <$> decoratorKeys


_findItemByIndex
    :: forall subj_id @subj @group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> Builder subj group item
    -> Maybe item
_findItemByIndex subjId groupPath itemIdx =
    Builder.directItemsOf subjId groupPath
        >>> fromMaybe []
        >>> flip Array.index itemIdx


nextInlinePos
    :: forall subj_id @item_tag subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasDecorators item
    => R.HasTags item_tag item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> PosKey
    -> Builder subj group item
    -> Maybe PosKey
nextInlinePos subjId groupPath itemIdx posKey builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (_itemMap @item_tag >>> Array.nextTo posKey)


previousInlinePos
    :: forall subj_id @item_tag subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasDecorators item
    => R.HasTags item_tag item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> PosKey
    -> Builder subj group item
    -> Maybe PosKey
previousInlinePos subjId groupPath itemIdx posKey builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (_itemMap @item_tag >>> Array.prevTo posKey)



nextTabular
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasTabular item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> TabularIndex
    -> Builder subj group item
    -> Maybe TabularIndex
nextTabular subjId groupPath itemIdx tabularIdx builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (R.i_tabular >>> Tab.items >>> Array.nextIndex tabularIdx)



previousTabular
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasTabular item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> TabularIndex
    -> Builder subj group item
    -> Maybe TabularIndex
previousTabular subjId groupPath itemIdx tabularIdx builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (R.i_tabular >>> Tab.items >>> Array.prevIndex tabularIdx)


firstSubject
    :: forall @subj_id subj group item
     . R.IsSubjectId subj_id subj
    => Builder subj group item
    -> Maybe subj_id
firstSubject =
    Builder.allSubjects >>> Array.head >>> map R.s_id


lastSubject
    :: forall @subj_id subj group item
     . R.IsSubjectId subj_id subj
    => Builder subj group item
    -> Maybe subj_id
lastSubject =
    Builder.allSubjects >>> Array.last >>> map R.s_id


firstGroupInSubj
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> Builder subj group item
    -> Maybe GroupPath
firstGroupInSubj subjId builder =
    Array.head (Builder.allGroupsOf subjId builder)
    <#> R.g_path


lastGroupInSubj
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> Builder subj group item
    -> Maybe GroupPath
lastGroupInSubj subjId builder =
    Array.last (Builder.allGroupsOf subjId builder)
    <#> R.g_path



firstItemInGroup
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Builder subj group item
    -> Maybe ItemIndex
firstItemInGroup subjId groupPath builder =
    Builder.directItemsOf subjId groupPath builder
    >>= (Array.length >>> \l -> if l > 0 then Just 0 else Nothing)



lastItemInGroup
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => subj_id
    -> GroupPath
    -> Builder subj group item
    -> Maybe ItemIndex
lastItemInGroup subjId groupPath builder =
    Builder.directItemsOf subjId groupPath builder
    >>= (Array.length >>> \l -> if l > 0 then Just $ l - 1 else Nothing)



firstTabularInItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasTabular item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> Builder subj group item
    -> Maybe TabularIndex
firstTabularInItem subjId groupPath itemIdx builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (R.i_tabular >>> Tab.items >>> Array.length >>> \l -> if l > 0 then Just 0 else Nothing)


lastTabularInItem
    :: forall subj_id subj group item
     . Eq subj_id
    => R.IsSubjectId subj_id subj
    => R.IsGroup group
    => R.HasTabular item
    => subj_id
    -> GroupPath
    -> ItemIndex
    -> Builder subj group item
    -> Maybe TabularIndex
lastTabularInItem subjId groupPath itemIdx builder =
    _findItemByIndex subjId groupPath itemIdx builder
    >>= (R.i_tabular >>> Tab.items >>> Array.length >>> \l -> if l > 0 then Just $ l - 1 else Nothing)