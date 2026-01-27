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
    , nodeToString
    , TreeNode(..)
    , class ToReport, toReport
    , withGroup, withItem
    , findGroup, findItem
    , leaveOnly, leaveOnlyById
    , collectSubjectsTags, collectItemsTags
    , filterItemsByTag, sortItemsByTag, groupItemsByTag
    )
    where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (toUnfoldable) as Set
import Data.Map (Map)
import Data.Map (empty, keys, fromFoldable, lookup, toUnfoldable, filterKeys, insert, alter, values) as Map
import Data.Map.Extra (lookupByEq, lookupByEq') as Map
import Data.Array (index, catMaybes, snoc, updateAt, concat, filter, elem, sortWith, find, nub) as Array
import Data.Array.Extra (groupExtBy) as Array
import Data.List (toUnfoldable) as List
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)


import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, build) as Tree

import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq, pathFromArray) as GPath
import Report.Class
import Report.Chain (toArray) as Chain


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


data TreeNode subj group item
    = NRoot
    | NSubject subj
    | NGroup group
    | NItem item


data TreeBuildStep subj group item
    = Start
    | Subj subj
    | Group group (Array item) (Array (TreeBuildStep subj group item))
    | Item item


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


toTree :: forall subj group item. Ord subj => Report subj group item -> Tree (TreeNode subj group item)
toTree (Report subjMap) =
    Tree.build buildF Start
    where
        makeGroupsSteps :: GroupsMap group item -> Array (TreeBuildStep subj group item)
        makeGroupsSteps subjGroups@(_ /\ pathToGItems) = Array.catMaybes (makeGroupStep 1 subjGroups <$> Map.toUnfoldable pathToGItems)
        makeGroupStep :: Int -> GroupsMap group item -> GroupPath /\ Array item -> Maybe (TreeBuildStep subj group item)
        makeGroupStep curLevel subjGroups@(pathToGroup /\ pathToGItems) (groupPath /\ items) =
            if GPath.howDeep groupPath == curLevel then
                Map.lookup groupPath pathToGroup
                <#>
                (\group ->
                    Group group items
                    $ Array.catMaybes
                        (makeGroupStep (curLevel + 1) subjGroups
                            <$> (Map.toUnfoldable $ Map.filterKeys (GPath.startsWithNotEq groupPath) pathToGItems)
                        )
                )
            else
                Nothing
        buildF = case _ of
            Start -> NRoot /\ (Subj <$> (Set.toUnfoldable $ Map.keys subjMap)) -- TODO: sort?
            Subj subj -> NSubject subj /\ (fromMaybe [] $ makeGroupsSteps <$> Map.lookup subj subjMap)
            Group group items steps -> NGroup group /\ ((Item <$> items) <> steps)
            Item item -> NItem item /\ []


instance (Show subj, Show group, Show item) => Show (TreeNode subj group item) where
    show = nodeToString false


nodeToString :: forall subj group item. Show subj => Show group => Show item => Boolean -> TreeNode subj group item -> String
nodeToString withPrefix = case _ of
    NRoot -> "*"
    NSubject subj -> if withPrefix then "S: " <> show subj  else show subj
    NGroup group  -> if withPrefix then "G: " <> show group else show group
    NItem item    -> if withPrefix then "I: " <> show item  else show item


fromTree :: forall a subj group item. Ord subj => Ord group => IsGroup group => (a -> TreeNode subj group item) -> Tree a -> Report subj group item
fromTree toNode =
    Tree.break
        (\root subjects ->
            Report $ Map.fromFoldable $ Array.catMaybes $ Tree.break breakSubjectF <$> subjects
        )
    where
        breakSubjectF a children =
            case toNode a of
                NSubject subj ->
                    Just $
                        subj /\ foldl (foldGroupsF $ GPath.pathFromArray []) (Map.empty /\ Map.empty) children
                _ -> Nothing
        foldGroupsF curPath (groupsMap /\ itemsMap) =
            Tree.break $ \a children -> case toNode a of
                NGroup grp ->
                    foldl
                        (foldGroupsF $ g_path grp)
                        (Map.insert (g_path grp) grp groupsMap /\ itemsMap)
                        children
                NItem item ->
                    foldl
                        (foldGroupsF curPath)
                        (groupsMap /\ Map.alter (addItem item) curPath itemsMap)
                        children
                _ -> groupsMap /\ itemsMap
        addItem item (Just arr) = Just $ Array.snoc arr item
        addItem item Nothing = Just $ pure item


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