module Report
    ( Report
    , ReportMap
    , TransferMap
    , GroupsMap
    , empty
    , toMap
    , unwrap
    , build
    , fromMap
    , toTree
    , fromTree
    , fromTreeC
    , class ToReport, toReport
    , withGroup, withItem
    , findGroup, findItem
    , leaveOnly, leaveOnlyById
    , collectSubjectsTags, collectItemsTags
    , filterItemsByTag, sortItemsByTag, groupItemsByTag
    )
    where

import Prelude

import Data.Maybe (Maybe, fromMaybe)
import Data.Set (toUnfoldable) as Set
import Data.Map (Map)
import Data.Map (empty, keys, fromFoldable, lookup, toUnfoldable, filterKeys, insert, values) as Map
import Data.Map.Extra (lookupByEq, lookupByEq') as Map
import Data.Array (index, catMaybes, updateAt, concat, filter, elem, sortWith, find, nub) as Array
import Data.Array.Extra (groupExtBy) as Array
import Data.List (toUnfoldable) as List
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)


import Yoga.Tree (Tree)

import Report.GroupPath (GroupPath)
import Report.Class
import Report.Chain (Chain(..), toArray) as Chain
import Report.Builder as B


type GroupsMap group item = Map GroupPath group /\ Map GroupPath (Array item)


type ReportMap subj group item = Map subj (GroupsMap group item)


type TransferMap subj group item = Map subj (Map group (Array item))


class ToReport subj group item a where
    toReport :: a -> Report subj group item


newtype Report subj group item =
    Report (ReportMap subj group item)


instance ToReport subj group item (Report subj group item) where
    toReport = identity


empty :: forall subj group item. Report subj group item
empty = Report Map.empty


unwrap
    :: forall subj group item
     . Report subj group item
    -> ReportMap subj group item
unwrap (Report subjsMap) = subjsMap


build :: forall subj group item. Ord subj => IsGroup group => Array (subj /\ Array (group /\ Array item)) -> Report subj group item
build subjects =
    Report $ Map.fromFoldable $ map splitGroupsMaps <$> subjects
    where
        splitGroupsMaps :: Array (group /\ Array item) -> GroupsMap group item
        splitGroupsMaps groupsArr =
            (Map.fromFoldable $ (\grp -> g_path grp /\ grp) <$> Tuple.fst <$> groupsArr)
            /\
            (Map.fromFoldable $ lmap g_path <$> groupsArr)


toMap
    :: forall subj group item
     . Ord group
    => Report subj group item
    -> TransferMap subj group item
toMap (Report subjsMap) =
    subjGroups <$> subjsMap
    where
        findGroup' :: Map GroupPath group -> GroupPath -> Maybe group
        findGroup' pathToGroup groupPath =
            Map.lookup groupPath pathToGroup
        wrapGroup :: Map GroupPath group -> GroupPath /\ Array item -> Maybe (group /\ Array item)
        wrapGroup pathToGroup (groupPath /\ items) = findGroup' pathToGroup groupPath <#> flip (/\) items
        subjGroups (pathToGroup /\ pathToGroupItems) =
            Map.fromFoldable $ Array.catMaybes $ wrapGroup pathToGroup <$> Map.toUnfoldable pathToGroupItems


fromMap :: forall subj group item. Ord subj => IsGroup group => TransferMap subj group item -> Report subj group item
fromMap = build <<< map (map Map.toUnfoldable) <<< Map.toUnfoldable


unfold :: forall subj group item. Report subj group item -> Array (subj /\ Array (group /\ Array item))
unfold (Report subjMap) = map unfoldSubj <$> Map.toUnfoldable subjMap
    where
        unfoldSubj (groupsMap /\ itemsMap) = Array.catMaybes $ unfoldGroup groupsMap <$> Map.toUnfoldable itemsMap
        unfoldGroup groupsMap (groupPath /\ items) =
            Map.lookup groupPath groupsMap <#> \group -> group /\ items


toBuilder :: forall subj group item. Report subj group item -> B.Builder subj group item
toBuilder =
    B.toBuilder <<< unfold


fromBuilder :: forall subj group item. Ord subj => IsGroup group => B.Builder subj group item -> Report subj group item
fromBuilder = case _ of
    B.Builder subjectsArr ->
        Report $ Map.fromFoldable $ mkSubject <$> subjectsArr
    where
        mkSubject = case _ of
            B.Subject subj groupsArr ->
                subj
                /\ (Map.fromFoldable $ Array.concat $ mkGroup <$> groupsArr)
                /\ (Map.fromFoldable $ Array.concat $ mkGroupWithItems  <$> groupsArr)
        mkGroup = case _ of
            B.Group groupC itemsArr ->
                ((\g -> g_path g /\ g) <$> Chain.toArray groupC)
        mkGroupWithItems = case _ of
            B.Group groupC itemsArr ->
                foldChain groupC itemsArr
        foldChain groupC items =
            case groupC of
                Chain.End g ->
                    [ g_path g /\ ((\(B.Item i) -> i) <$> items) ]
                Chain.More g restC ->
                    [ g_path g /\ [] ] <> foldChain restC items


toTree :: forall subj group item. Ord subj => Report subj group item -> Tree (B.TreeNode subj group item)
toTree =
    toBuilder >>> B.toTree


fromTree :: forall a subj group item. Ord subj => Ord group => IsGroup group => (a -> B.TreeNode subj group item) -> Tree a -> Report subj group item
fromTree toNode =
    map toNode >>> B.fromTree >>> fromBuilder


fromTreeC :: forall a subj group item. Ord subj => Ord group => IsGroup group => (a -> B.TreeNodeC subj group item) -> Tree a -> Report subj group item
fromTreeC toNode =
    map toNode >>> B.fromTreeC >>> fromBuilder


withGroup
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => Ord subj_id
    => Ord subj
    => subj_id
    -> GroupPath
    -> (group -> group)
    -> Report subj group item
    -> Maybe (Report subj group item)
withGroup subjId groupPath f (Report subjMap) = do
    subj /\ pathToGroup /\ pathToItems <- Map.lookupByEq' s_id subjId subjMap
    curGroup <- Map.lookup groupPath pathToGroup
    let nextPathToGroup = Map.insert groupPath (f curGroup) pathToGroup
    let nextSubjMap = Map.insert subj (nextPathToGroup /\ pathToItems) subjMap
    pure $ Report nextSubjMap


withItem
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => Ord subj_id
    => Ord subj
    => subj_id
    -> GroupPath
    -> Int
    -> (item -> item)
    -> Report subj group item
    -> Maybe (Report subj group item)
withItem subjId groupPath itemIdx f (Report subjMap) = do
    subj /\ pathToGroup /\ pathToItems <- Map.lookupByEq' s_id subjId subjMap
    itemsArr <- Map.lookup groupPath pathToItems
    curItem <- Array.index itemsArr itemIdx
    nextItemsArr <- Array.updateAt itemIdx (f curItem) itemsArr
    let nextPathToItems = Map.insert groupPath nextItemsArr pathToItems
    let nextSubjMap = Map.insert subj (pathToGroup /\ nextPathToItems) subjMap
    pure $ Report nextSubjMap


findGroup
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => Ord subj_id
    => Ord subj
    => subj_id
    -> GroupPath
    -> Report subj group item
    -> Maybe group
findGroup subjId groupPath (Report subjMap) = do
    Map.lookupByEq s_id subjId subjMap
        <#> Tuple.fst
        >>= Map.lookup groupPath


findItem
    :: forall subj_id subj group item
     . IsSubjectId subj_id subj
    => Ord subj_id
    => Ord subj
    => subj_id
    -> GroupPath
    -> Int
    -> Report subj group item
    -> Maybe item
findItem subjId groupPath itemIdx (Report subjMap) =
    Map.lookupByEq s_id subjId subjMap
        <#> Tuple.snd
        >>= Map.lookup groupPath
        >>= flip Array.index itemIdx


leaveOnly :: forall subj group item. Ord subj => Array subj -> Report subj group item -> Report subj group item
leaveOnly toFilter =
    unwrap
        >>> Map.filterKeys (flip Array.elem toFilter)
        >>> Report


leaveOnlyById :: forall subj_id subj group item. Ord subj => Ord subj_id => IsSubjectId subj_id subj => Array subj_id -> Report subj group item -> Report subj group item
leaveOnlyById toFilter =
    unwrap
        >>> Map.filterKeys (s_id >>> flip Array.elem toFilter)
        >>> Report


collectSubjectsTags
    :: forall subj_tag subj group item
     . Ord subj_tag
    => HasTags subj_tag subj
    => Report subj group item
    -> Array subj_tag
collectSubjectsTags =
    unwrap
        >>> Map.keys
        >>> Set.toUnfoldable
        >>> map i_tags
        >>> Array.concat
        >>> Array.nub


collectItemsTags
    :: forall item_tag subj group item
     . Ord item_tag
    => HasTags item_tag item
    => Report subj group item
    -> Array item_tag
collectItemsTags =
    unwrap
        >>> Map.values
        >>> map Tuple.snd
        >>> List.toUnfoldable
        >>> map (Map.values >>> List.toUnfoldable)
        >>> Array.concat
        >>> Array.concat
        >>> map i_tags
        >>> Array.concat
        >>> Array.nub


filterItemsByTag
    :: forall item_tag subj group item
     . Eq item_tag
    => HasTags item_tag item
    => item_tag
    -> Report subj group item
    -> Report subj group item
filterItemsByTag itemTag = unwrap >>> map filterGroups >>> Report
    where
        filterGroups :: GroupsMap group item -> GroupsMap group item
        filterGroups (pathToGroup /\ pathToItems) =
            pathToGroup /\ ((Array.filter $ hasTag itemTag) <$> pathToItems)
        hasTag :: item_tag -> item -> Boolean
        hasTag tag = i_tags >>> Array.elem tag


sortItemsByTag
    :: forall item_tag subj group item
     . Ord item_tag
    => HasTags item_tag item
    => IsSortable item_tag
    => item_tag
    -> Report subj group item
    -> Report subj group item
sortItemsByTag itemTag = unwrap >>> map sortGroups >>> Report
    where
        sortGroups :: GroupsMap group item -> GroupsMap group item
        sortGroups (pathToGroup /\ pathToItems) =
            pathToGroup /\ ((Array.sortWith $ itemTagSortValue itemTag) <$> pathToItems)
        itemTagSortValue :: item_tag -> item -> Maybe item_tag
        itemTagSortValue tag =
            Array.find (sameKind tag) <<< i_tags


groupItemsByTag
    :: forall item_tag subj group item
     . Ord item_tag
    => HasTags item_tag item
    => IsGroupable group item_tag
    => IsSortable item_tag
    => item_tag
    -> Report subj group item
    -> Report subj group item
groupItemsByTag itemTag = unwrap >>> map regroupItems >>> Report
    where
        regroupItems :: GroupsMap group item -> GroupsMap group item
        regroupItems (_ /\ pathToItems) =
            let
                groupToItems = regroupToArray pathToItems
                regroupedItems = Map.fromFoldable $ lmap g_path <$> groupToItems
            in newPathToGroup (Tuple.fst <$> groupToItems) /\ regroupedItems
        newPathToGroup :: Array group -> Map GroupPath group
        newPathToGroup = map (\grp -> g_path grp /\ grp) >>> Map.fromFoldable
        regroupToArray :: Map GroupPath (Array item) -> Array (group /\ Array item)
        regroupToArray = Map.values >>> List.toUnfoldable >>> Array.concat >>> regroupItemsArr
        regroupItemsArr :: Array item -> Array (group /\ Array item)
        regroupItemsArr =
            map
                (\item ->
                    let sameKindTags = Array.filter (sameKind itemTag) $ i_tags item
                    in (\itag -> t_group @group itag <#> Chain.toArray <#> map (flip (/\) item)) <$> sameKindTags
                )
            >>> map ((map $ fromMaybe []) >>> Array.concat)
            >>> Array.concat
            -- >>> Array.catMaybes
            >>> Array.groupExtBy (\ga gb -> compare (g_path ga) (g_path gb)) Tuple.fst Tuple.snd