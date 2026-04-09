module Report
    ( Report
    , empty
    , build
    , toTree
    , toBuilder, fromBuilder
    , unfold, unfoldAll
    -- , fromTreeC
    , class ToReport, toReport
    , withGroup, withItem
    , findGroup, findItem
    , leaveOnly, leaveOnlyById
    , collectSubjectsTags, collectItemsTags
    , filterItemsByTag, sortItemsByTag, groupItemsByTag
    , module BExport
    )
    where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (toUnfoldable) as Set
import Data.Map (Map)
import Data.Map (empty, keys, fromFoldable, lookup, toUnfoldable, filterKeys, insert, values) as Map
import Data.Map.Extra (lookupByEq, lookupByEq') as Map
import Data.Array (index, catMaybes, updateAt, concat, filter, elem, sortWith, find, findMap, nub) as Array
import Data.Array.Extra (groupExtBy) as Array
import Data.List (toUnfoldable) as List
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)


import Yoga.Tree (Tree)

import Report.GroupPath (GroupPath)
import Report.Class
import Report.Chain (Chain)
import Report.Chain (Chain(..), toArray, last) as Chain
import Report.Builder as B
import Report.Builder
    ( Builder(..)
    , Subject(..)
    , Group(..)
    , Item(..)
    , TreeNode(..)
    -- , toBuilder, toBuilderC
    -- , unfold, unfoldC
    -- , toTree
    , nodeToString
    {- Subjects -}
    , mapSubjects
    , allSubjects
    , filterSubjects
    , sortSubjects, sortSubjectsWith, sortSubjectsBy, sortSubjectsByWith
    {- Groups -}
    , mapGroups
    , allGroups, allGroupsC
    , filterGroups
    -- , withGroup, withGroupIdx
    -- , findGroup, findMapGroup
    , sortGroups, sortGroupsWith, sortGroupsBy
    , regroup, regroupBy, regroupByMany
    {- Items -}
    , mapItems
    , allItems
    , filterItems
    -- , withItem, withItemIdx
    , sortItems, sortItemsWith, sortItemsBy, sortItemsByWith
    -- , findItem, findMapItem, findMapItem'
    ) as BExport


class ToReport subj group item a where
    toReport :: a -> Report subj group item


newtype Report subj group item =
    Report (B.Builder subj group item)


instance ToReport subj group item (Report subj group item) where
    toReport = identity


empty :: forall subj group item. Report subj group item
empty = Report B.empty


-- unwrap
--     :: forall subj group item
--      . Report subj group item
--     -> ReportMap subj group item
-- unwrap (Report subjsMap) = subjsMap


build :: forall subj group item. Array (subj /\ Array (group /\ Array item)) -> Report subj group item
build = Report <<< B.build


toBuilder :: forall subj group item. Report subj group item -> B.Builder subj group item
toBuilder (Report builder) = builder


unfold :: forall subj group item. Report subj group item -> Array (subj /\ Array (group /\ Array item))
unfold = toBuilder >>> B.unfold


unfoldAll :: forall subj group item. Ord group => Report subj group item -> Array (subj /\ Array (group /\ Array item))
unfoldAll = toBuilder >>> B.unfoldAll


fromBuilder :: forall subj group item. B.Builder subj group item -> Report subj group item
fromBuilder = Report


toTree :: forall subj group item. Ord group => Report subj group item -> Tree (B.TreeNode subj group item)
toTree =
    toBuilder >>> B.toTree


-- fromTree :: forall a subj group item. IsGroup group => (a -> B.TreeNode subj group item) -> Tree a -> Report subj group item
-- fromTree toNode =
--     map toNode >>> B.fromTree >>> fromBuilder


{-
fromTreeC :: forall a subj group item. Ord subj => Ord group => IsGroup group => (a -> B.TreeNodeC subj group item) -> Tree a -> Report subj group item
fromTreeC toNode =
    map toNode >>> B.fromTreeC >>> fromBuilder
-}


withGroup
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => IsGroup group
    => Eq subj_id
    => subj_id
    -> GroupPath
    -> (group -> group)
    -> Report subj group item
    -> Report subj group item
withGroup subjId groupPath f =
    toBuilder
        >>> B.withGroup
            (\otherSubj otherGrp ->
                if ((s_id otherSubj == subjId) && (g_path otherGrp == groupPath)) then f otherGrp
                else otherGrp
            )
        >>> fromBuilder
    {-
    subj /\ pathToGroup /\ pathToItems <- Map.lookupByEq' s_id subjId subjMap
    curGroup <- Map.lookup groupPath pathToGroup
    let nextPathToGroup = Map.insert groupPath (f curGroup) pathToGroup
    let nextSubjMap = Map.insert subj (nextPathToGroup /\ pathToItems) subjMap
    pure $ Report nextSubjMap
    -}


withItem
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => IsGroup group
    => Eq subj_id
    => subj_id
    -> GroupPath
    -> Int
    -> (item -> item)
    -> Report subj group item
    -> Report subj group item
withItem subjId groupPath itemIdx f =
    toBuilder
        >>> B.withItemIdx
            (\otherSubj groupC otherIdx otherItem ->
                if (s_id otherSubj == subjId) then
                    if (g_path (Chain.last groupC) == groupPath) then
                        if (itemIdx == otherIdx) then f otherItem
                        else otherItem
                    else otherItem
                else otherItem
            )
        >>> fromBuilder


findGroup
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => IsGroup group
    => Eq subj_id
    => subj_id
    -> GroupPath
    -> Report subj group item
    -> Maybe group
findGroup subjId groupPath = do
    toBuilder >>> B.findMapGroup
        \subj groupC ->
            if (s_id subj == subjId) then
                Array.find (g_path >>> (_ == groupPath)) $ Chain.toArray groupC
            else Nothing


findItem
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => IsGroup group
    => Eq subj_id
    => subj_id
    -> GroupPath
    -> Int
    -> Report subj group item
    -> Maybe item
findItem subjId groupPath itemIdx =
    toBuilder >>> B.mapGroups g_path >>> B.findMapItems
        \subj groupC items ->
            if (s_id subj == subjId) && (Chain.last groupC == groupPath)
                then Array.index items itemIdx
                else Nothing
    {-
    Map.lookupByEq s_id subjId subjMap
        <#> Tuple.snd
        >>= Map.lookup groupPath
        >>= flip Array.index itemIdx
    -}


leaveOnly :: forall subj group item. Eq subj => Array subj -> Report subj group item -> Report subj group item
leaveOnly toFilter =
    toBuilder >>> B.filterSubjects (flip Array.elem toFilter) >>> fromBuilder


leaveOnlyById :: forall subj_id subj group item. Eq subj_id => IsSubjectId subj_id subj => Array subj_id -> Report subj group item -> Report subj group item
leaveOnlyById toFilter =
    toBuilder >>> B.filterSubjects (s_id >>> flip Array.elem toFilter) >>> fromBuilder


collectSubjectsTags
    :: forall subj_tag subj group item
     . Ord subj_tag
    => HasTags subj_tag subj
    => Report subj group item
    -> Array subj_tag
collectSubjectsTags =
    toBuilder >>> B.allSubjects >>> map i_tags >>> Array.concat >>> Array.nub


collectItemsTags
    :: forall item_tag subj group item
     . Ord item_tag
    => HasTags item_tag item
    => Report subj group item
    -> Array item_tag
collectItemsTags =
    toBuilder >>> B.allItems >>> map i_tags >>> Array.concat >>> Array.nub


filterItemsByTag
    :: forall item_tag subj group item
     . Eq item_tag
    => HasTags item_tag item
    => item_tag
    -> Report subj group item
    -> Report subj group item
filterItemsByTag itemTag =
    toBuilder >>> B.filterItems (const $ const $ i_tags >>> Array.elem itemTag) >>> fromBuilder


sortItemsByTag
    :: forall tag_kind @item_tag subj group item
     . Ord item_tag
    => HasTags item_tag item
    => IsSortable tag_kind item_tag
    => tag_kind
    -> Report subj group item
    -> Report subj group item
sortItemsByTag itemTagKind =
    toBuilder >>> B.sortItemsWith (itemTagSortValue itemTagKind) >>> Report
    where
        itemTagSortValue :: tag_kind -> item -> Maybe item_tag
        itemTagSortValue tagKind =
            Array.find (kindOf @tag_kind >>> same tagKind) <<< i_tags


groupItemsByTag
    :: forall tag_kind @item_tag subj group item
     . HasTags item_tag item
    => IsGroupable group item_tag
    => IsSortable tag_kind item_tag
    => tag_kind
    -> Report subj group item
    -> Report subj group item
groupItemsByTag itemTagKind =
    toBuilder
        >>> B.regroupByMany
            (\chA chB -> compare (g_path <$> Chain.toArray chA) (g_path <$> Chain.toArray chB))
            (\item ->
                let sameKindTags = Array.filter (kindOf @tag_kind @item_tag >>> same itemTagKind) $ i_tags item
                in Array.catMaybes $ (\otherTag -> t_group @group otherTag) <$> sameKindTags
            )
        >>> fromBuilder