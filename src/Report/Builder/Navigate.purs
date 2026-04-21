module Report.Builder.Navigate where


import Prelude

import Data.Maybe
import Data.Newtype
import Data.Array as Array
import Data.Array.Extra as Array

import Report.Builder
import Report.Builder as Builder
import Report.Class as R
import Report.GroupPath
import Report.Decorator as Decorator
import Report.Decorator (Key(..)) as D
import Report.Tabular (items) as Tab


type ItemIndex = Int
type TabularIndex = Int


data PosKey
    = PKRating
    | PKPriority
    | PKTask
    | PKItemName
    | PKProgress
    | PKEarnedAt
    | PKDescription
    | PKReference
    | PKTags


derive instance Eq PosKey
instance Ord PosKey where
    compare pkA pkB = orderOf pkA `compare` orderOf pkB


toPosKey :: Decorator.Key -> PosKey
toPosKey = case _ of
    D.KRating -> PKRating
    D.KPriority -> PKPriority
    D.KTask -> PKTask
    D.KProgress _ -> PKProgress -- handle tasks inside progress?
    D.KEarnedAt -> PKEarnedAt
    D.KDescription -> PKDescription
    D.KReference -> PKReference


nextPos :: PosKey -> Maybe PosKey
nextPos = case _ of
    PKRating -> Just PKPriority
    PKPriority -> Just PKTask
    PKTask -> Just PKItemName
    PKItemName -> Just PKProgress
    PKProgress -> Just PKEarnedAt
    PKEarnedAt -> Just PKDescription
    PKDescription -> Just PKReference
    PKReference -> Just PKTags
    PKTags -> Nothing


prevPos :: PosKey -> Maybe PosKey
prevPos = case _ of
    PKRating -> Nothing
    PKPriority -> Just PKRating
    PKTask -> Just PKPriority
    PKItemName -> Just PKTask
    PKProgress -> Just PKItemName
    PKEarnedAt -> Just PKProgress
    PKDescription -> Just PKEarnedAt
    PKReference -> Just PKDescription
    PKTags -> Just PKReference


orderOf :: PosKey -> Int
orderOf = case _ of
    PKRating -> -3
    PKPriority -> -2
    PKTask -> -1
    PKItemName -> 0
    PKProgress -> 1
    PKEarnedAt -> 2
    PKDescription -> 3
    PKReference -> 4
    PKTags -> 5


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
    :: forall subj_id subj group item
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


{-
firstTabularInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Int
firstTabularInItem = ?wg


lastTabularInItem :: forall subj_id subj group item. subj_id -> GroupPath -> Int -> Builder subj group item -> Maybe Int
lastTabularInItem = ?wg
-}