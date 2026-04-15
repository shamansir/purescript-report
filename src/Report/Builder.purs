module Report.Builder
    ( Builder(..)
    , Subject(..)
    , Group(..)
    , Item(..)
    , TreeNode(..)
    , empty
    , build, buildG
    , toBuilder, toBuilderC, toBuilderG
    , unfold, unfoldC, unfoldAll
    , toTree, toTree_
    , nodeToString
    {- Subjects -}
    , mapSubjects
    , allSubjects
    , filterSubjects
    , sortSubjects, sortSubjectsWith, sortSubjectsBy, sortSubjectsByWith
    , redistribute, redistributeBy
    , alignSubjects, alignSubjectsBy
    {- Groups -}
    , mapGroups
    , allGroups, allGroupsC
    , allGroupsOf, allGroupsOfC
    , filterGroups
    , withGroup, withGroupIdx
    , findGroup, findMapGroup
    , sortGroups, sortGroupsWith, sortGroupsBy
    , regroup, regroupBy, regroupByMany
    , mapGroupsWithItems, mapGroupsWithItemsE, mapGroupsWithItemsC
    {- Items -}
    , mapItems
    , allItems
    , filterItems
    , withItem, withItemIdx
    , sortItems, sortItemsWith, sortItemsBy, sortItemsByWith
    , findItem, findMapItem, findMapItems
    , directItemsOf, allSubjectsItems, allSubjectsItems_, allGroupsItems, allGroupsItems_
    ) where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Either (Either(..), either)
import Data.Foldable (foldl)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (head, concat, catMaybes, sort, sortWith, sortBy, groupAll, groupAllBy, filter, find, findMap, concatMap, mapMaybe) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, toArray, fromArray) as NEA
import Data.Array.Extra (groupExt, groupExtBy) as Array
import Data.String (joinWith) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Bifunctor (class Bifunctor, rmap, lmap)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Map (Map)
import Data.Map as Map

import Report.Class as RC
import Report.Chain (Chain)
import Report.Chain as Chain
import Report.GroupPath (GroupPath)
import Report.GroupPath as GP

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, leaf, break, build, regroup, regroupByPath) as Tree


newtype Builder s g i = Builder (Array (Subject s g i))
data Subject s g i = Subject s (Array (Group g i))
data Group g i = Group (Chain g) (Array (Item i))
newtype Item i = Item i


derive instance Functor Item
derive instance Functor (Group g)
derive instance Functor (Subject s g)
derive instance Functor (Builder subj group)

derive instance Bifunctor Group
derive instance Bifunctor (Subject s)
derive instance Bifunctor (Builder subj)

derive instance Newtype (Builder s g i) _
derive instance Newtype (Item i) _


derive instance Eq a => Eq (Item a)
derive instance Ord a => Ord (Item a)


data TreeNode subj group item  -- both as a source when converting from `Tree` or a target when converting to `Tree`
    = NRoot
    | NSubject subj
    | NGroup group
    | NItem item


data TreeNodeC subj group item -- only as a source when converting from `Tree`
    = CNRoot
    | CNSubject subj
    | CNGroup (Chain group)
    | CNItem item


derive instance Functor (TreeNode subj group)
derive instance Bifunctor (TreeNode subj)


derive instance Functor (TreeNodeC subj group)
derive instance Bifunctor (TreeNodeC subj)


empty :: forall subj group item. Builder subj group item
empty = Builder []


build :: forall subj group item. Array (subj /\ Array (group /\ Array item)) -> Builder subj group item
build = toBuilder


buildG :: forall subj group item. RC.IsGroup group => Array (subj /\ Array (group /\ Array item)) -> Builder subj group item
buildG = toBuilderG


toBuilder :: forall subj group item. Array (subj /\ Array (group /\ Array item)) -> Builder subj group item
toBuilder = map (map $ map $ lmap Chain.singleton) >>> toBuilderC


toBuilderC :: forall subj group item. Array (subj /\ Array (Chain group /\ Array item)) -> Builder subj group item
toBuilderC subjects =
    Builder $ toSubjectR <$> subjects
    where
        toSubjectR :: subj /\ Array (Chain group /\ Array item) -> Subject subj group item
        toSubjectR (subj /\ groupsArr) =
            Subject subj $ toGroupR <$> groupsArr
        toGroupR :: Chain group /\ Array item -> Group group item
        toGroupR (group /\ itemsArr) =
            Group group (Item <$> itemsArr)


toBuilderG :: forall subj group item. RC.IsGroup group => Array (subj /\ Array (group /\ Array item)) -> Builder subj group item
toBuilderG = map (map makeChains) >>> toBuilderC
    where
        makeChains :: Array (group /\ Array item) -> Array (Chain group /\ Array item)
        makeChains groups =
            let
                groupWithPath grp = RC.g_path grp /\ grp
                (pathToGroup :: Map GroupPath group) = Map.fromFoldable $ groupWithPath <$> Tuple.fst <$> groups
                groupToChain :: group -> Chain group
                groupToChain grp =
                    let
                        (curGroupPathChain :: Maybe (Chain String)) = Chain.fromArray $ GP.pathToArray $ RC.g_path grp
                        (groupsOnTheWayRaw :: Array (Chain String)) = fromMaybe [] $ Chain.allIn <$> curGroupPathChain
                        (groupsOnTheWay :: Array GroupPath) = (Chain.toArray >>> GP.pathFromArray) <$> groupsOnTheWayRaw
                    in fromMaybe (Chain.singleton grp) $ Chain.fromArray $ Array.catMaybes $ flip Map.lookup pathToGroup <$> groupsOnTheWay
            in lmap groupToChain <$> groups


unfold :: forall subj group item. Builder subj group item -> Array (subj /\ Array (group /\ Array item))
unfold = unfoldC >>> map (map $ map unfoldF)
    where
        unfoldF (gchain /\ items) = Chain.last gchain /\ items -- FIXME: only the `Chain.last` is used, is it a proper `unfold`?


unfoldAll :: forall subj group item. Ord group => Builder subj group item -> Array (subj /\ Array (group /\ Array item))
unfoldAll = unfoldC >>> map (map collectMap) >>> map (map Map.toUnfoldable)
    where
        collectMap = foldl fold1F Map.empty
        alterF items (Just prev) = Just $ prev <> items
        alterF items Nothing = Just items
        fold1F theMap (gchain /\ items) =
            (foldl (flip $ Map.alter $ alterF []) theMap $ Chain.beforeLast gchain)
                # Map.alter (alterF items) (Chain.last gchain)


unfoldC :: forall subj group item. Builder subj group item -> Array (subj /\ Array (Chain group /\ Array item))
unfoldC = unwrap >>> map \(Subject subj groups) -> subj /\ (unfoldGroup <$> groups)
    where
        unfoldGroup (Group groupC items) = groupC /\ (unwrap <$> items)


toTree :: forall subj group item. Builder subj group item -> Tree (TreeNode subj group item)
toTree =
    toTree_
        NRoot
        (\subj groups -> Tree.node (NSubject subj) groups)
        foldChain
    where
        foldChain :: Chain group -> Array item -> Tree (TreeNode subj group item)
        foldChain groupC subGoiArr =
            case groupC of
                Chain.End g ->
                    Tree.node (NGroup g) $ Tree.leaf <$> NItem <$> subGoiArr
                Chain.More g restC ->
                    Tree.node (NGroup g) $ pure $ foldChain restC subGoiArr


toTree_
    :: forall subj group item c
     . c
    -> (subj -> Array (Tree c) -> Tree c)
    -> (Chain group -> Array item -> Tree c)
    -> Builder subj group item
    -> Tree c
toTree_ root toSubjNode toGroupNode (Builder subjects) =
    Tree.node root $ toSubjectTree <$> subjects
    where
        toSubjectTree :: Subject subj group item -> Tree c
        toSubjectTree (Subject subj groupsArr) =
            toSubjNode subj $ toGroupTree <$> groupsArr
        toGroupTree :: Group group item -> Tree c
        toGroupTree goi =
            case goi of
                Group groupC goiArr ->
                    toGroupNode groupC $ unwrap <$> goiArr


{- TODO: remove? -}
fromTree :: forall subj group item. Tree (TreeNode subj group item) -> Builder subj group item
fromTree =
    map convertNode >>> fromTreeC
    where
        convertNode :: TreeNode subj group item -> TreeNodeC subj group item
        convertNode = case _ of
            NRoot -> CNRoot
            NSubject subj -> CNSubject subj
            NGroup group -> CNGroup $ Chain.end group
            NItem item -> CNItem item


{- TODO: remove? -}
fromTreeC :: forall subj group item. Tree (TreeNodeC subj group item) -> Builder subj group item
fromTreeC =
    Builder <<< fromSubjectTree
    where
        fromSubjectTree :: Tree (TreeNodeC subj group item) -> Array (Subject subj group item)
        fromSubjectTree =
            Tree.break \n arr -> case n of
                CNRoot ->
                    Array.catMaybes $ Tree.break breakSubjectF <$> arr
                CNSubject subj ->
                    pure $ Subject subj $ Array.catMaybes $ Tree.break breakGroupF <$> arr
                _ ->
                    []
        breakSubjectF a children =
            case a of
                CNSubject subj ->
                    Just $ Subject subj $ Array.catMaybes $ Tree.break breakGroupF <$> children
                _ -> Nothing
        breakGroupF a children =
            case a of
                CNGroup groupC ->
                    Just $ Group groupC $ Array.catMaybes $ Tree.break breakItemF <$> children
                _ -> Nothing
        breakItemF a _ =
            case a of
                CNItem item ->
                    Just $ Item item
                _ -> Nothing


instance (Show subj, Show group, Show item) => Show (TreeNode subj group item) where
    show = nodeToString false


nodeToString :: forall subj group item. Show subj => Show group => Show item => Boolean -> TreeNode subj group item -> String
nodeToString withPrefix = case _ of
    NRoot -> "*"
    NSubject subj -> if withPrefix then "S: " <> show subj  else show subj
    NGroup group  -> if withPrefix then "G: " <> show group else show group
    NItem item    -> if withPrefix then "I: " <> show item  else show item


nodeCToString :: forall subj group item. Show subj => Show group => Show item => Boolean -> TreeNodeC subj group item -> String
nodeCToString withPrefix = case _ of
    CNRoot -> "*"
    CNSubject subj -> if withPrefix then "S: " <> show subj  else show subj
    CNGroup groupC -> if withPrefix then "G: " <> (String.joinWith " ... " $ show <$> Chain.toArray groupC) else (String.joinWith " ... " $ show <$> Chain.toArray groupC)
    CNItem item    -> if withPrefix then "I: " <> show item  else show item

{- Subjects -}

mapSubjects :: forall subjA subjB group item. (subjA -> subjB) -> Builder subjA group item -> Builder subjB group item
mapSubjects mapSubj = unwrap >>> map mapFn >>> wrap
    where
        mapFn :: Subject subjA group item -> Subject subjB group item
        mapFn (Subject s groups) = Subject (mapSubj s) groups


sortSubjects :: forall subj group item. Ord subj => Builder subj group item -> Builder subj group item
sortSubjects = unwrap >>> Array.sortWith sortWithFn >>> wrap
    where
        sortWithFn (Subject s _) = s


sortSubjectsWith :: forall subj group item a. Ord a => (subj -> a) -> Builder subj group item -> Builder subj group item
sortSubjectsWith toA = unwrap >>> Array.sortWith sortWithFn >>> wrap
    where
        sortWithFn (Subject s _) = toA s


sortSubjectsBy :: forall subj group item. (subj -> subj -> Ordering) -> Builder subj group item -> Builder subj group item
sortSubjectsBy = sortSubjectsByWith identity


sortSubjectsByWith :: forall subj group item a. (subj -> a) -> (a -> a -> Ordering) -> Builder subj group item -> Builder subj group item
sortSubjectsByWith toA ordFn = unwrap >>> Array.sortBy sortByFn >>> wrap
    where
        sortByFn (Subject sA _) (Subject sB _) = ordFn (toA sA) (toA sB)


allSubjects :: forall subj group item. Builder subj group item -> Array subj
allSubjects = unwrap >>> map extractSubj
    where
        extractSubj (Subject s _) = s


filterSubjects :: forall subj group item. (subj -> Boolean) -> Builder subj group item -> Builder subj group item
filterSubjects filterF = unwrap >>> Array.filter subjSatisfy >>> wrap
    where
        subjSatisfy (Subject s _) = filterF s


redistribute :: forall subjA subjB group item. Ord subjB => (Chain group -> subjB) -> Builder subjA group item -> Builder subjB group item
redistribute = redistributeBy compare


redistributeBy :: forall subjA subjB group item. (subjB -> subjB -> Ordering) -> (Chain group -> subjB) -> Builder subjA group item -> Builder subjB group item
redistributeBy compareF toNewSubj =
    allGroupsWithItemsC
    >>> map (\(grpC /\ items) -> toNewSubj grpC /\ pure (grpC /\ items) )
    >>> toBuilderC
    >>> alignSubjectsBy compareF


-- FIXME: removes empty subjects due to the way `Array.group` works
-- TODO: implement the same for Groups
alignSubjects :: forall subj group item. Ord subj => Builder subj group item -> Builder subj group item
alignSubjects = alignSubjectsBy compare


alignSubjectsBy :: forall subj group item. (subj -> subj -> Ordering) -> Builder subj group item -> Builder subj group item
alignSubjectsBy compareF = unwrap >>> Array.groupExtBy compareF extractSubj extractGroups >>> map (map Array.concat >>> makeSubject) >>> wrap
    where
        extractSubj (Subject s _) = s
        extractGroups (Subject _ groups) = groups
        makeSubject (s /\ groups) = Subject s groups


{- Groups -}


mapGroups :: forall subj groupA groupB item. (groupA -> groupB) -> Builder subj groupA item -> Builder subj groupB item
mapGroups = lmap


--| Map groups with knowning their sub-items. The complete `Chain group` is updated using the mapping function.
mapGroupsWithItems :: forall subj groupA groupB item. (subj -> groupA -> Array item -> groupB) -> Builder subj groupA item -> Builder subj groupB item
mapGroupsWithItems mapFn = mapGroupsWithItemsC \subj groupC items -> flip (mapFn subj) items <$> groupC


--| Changes only the last group in the chain, that's why it is impossible to change group type
mapGroupsWithItemsE :: forall subj group item. (subj -> group -> Array item -> group) -> Builder subj group item -> Builder subj group item
mapGroupsWithItemsE mapFn = mapGroupsWithItemsC \subj groupC items ->
    let
        before /\ end = Chain.break groupC
    in Chain.make before $ mapFn subj end items


--| Map groups with knowning their sub-items.
mapGroupsWithItemsC :: forall subj groupA groupB item. (subj -> Chain groupA -> Array item -> Chain groupB) -> Builder subj groupA item -> Builder subj groupB item
mapGroupsWithItemsC mapF = unwrap >>> map mapGroupsF >>> wrap
    where
        mapGroupsF (Subject s groups) = Subject s $ mapGroupF s  <$> groups
        mapGroupF s (Group groupC items) = Group (mapF s groupC $ unwrap <$> items) items


withGroup :: forall subj groupA groupB item. (subj -> groupA -> groupB) -> Builder subj groupA item -> Builder subj groupB item
withGroup mapF = unwrap >>> map mapGroupsF >>> wrap
    where
        mapGroupsF (Subject s groups) = Subject s $ lmap (mapF s) <$> groups


withGroupIdx :: forall subj groupA groupB item. (subj -> Int -> groupA -> groupB) -> Builder subj groupA item -> Builder subj groupB item
withGroupIdx mapF = unwrap >>> map mapGroupsF >>> wrap
    where
        mapGroupsF (Subject s groups) = Subject s $ mapWithIndex (lmap <<< mapF s) groups


sortGroups :: forall subj group item. Ord group => Builder subj group item -> Builder subj group item
sortGroups = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ Array.sortWith sortWithFn groups
        sortWithFn (Group gchain _) = Chain.toArray gchain


sortGroupsWith :: forall subj group item a. Ord a => (group -> a) -> Builder subj group item -> Builder subj group item
sortGroupsWith toA = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ Array.sortWith sortWithFn groups
        sortWithFn (Group gchain _) = toA <$> Chain.toArray gchain


sortGroupsBy :: forall subj group item. (Chain group -> Chain group -> Ordering) -> Builder subj group item -> Builder subj group item
sortGroupsBy = sortGroupsByWith identity


sortGroupsByWith :: forall subj group item a. (group -> a) -> (Chain a -> Chain a -> Ordering) -> Builder subj group item -> Builder subj group item
sortGroupsByWith toA ordFn = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ Array.sortBy sortByFn groups
        sortByFn (Group gchainA _) (Group gchainB _) = ordFn (toA <$> gchainA) (toA <$> gchainB)


regroup :: forall subj groupA groupB item. Ord groupB => (item -> Chain groupB) -> Builder subj groupA item -> Builder subj groupB item
regroup = regroupBy compare


regroupBy :: forall subj groupA groupB item. (Chain groupB -> Chain groupB -> Ordering) -> (item -> Chain groupB) -> Builder subj groupA item -> Builder subj groupB item
regroupBy ordFn itemToGroup = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) =
            Subject s $ backToGroups $ Array.groupAllBy groupByFn $ allItemsOf groups

        allItemsOf :: Array (Group groupA item) -> Array (Chain groupB /\ Item item)
        allItemsOf = map (\(Group _ items) -> addGroup <$> items) >>> Array.concat

        addGroup (Item i) = itemToGroup i /\ Item i

        groupByFn :: (Chain groupB /\ Item item) -> (Chain groupB /\ Item item) -> Ordering
        groupByFn (gchainA /\ _) (gchainB /\ _) = ordFn gchainA gchainB

        backToGroups :: Array (NonEmptyArray (Chain groupB /\ Item item)) -> Array (Group groupB item)
        backToGroups =
            map (\nea -> (Tuple.fst $ NEA.head nea) /\ (Tuple.snd <$> NEA.toArray nea))
                >>> map (\(groupC /\ items) -> Group groupC items)


regroupByMany
    :: forall subj groupA groupB item
     . (Chain groupB -> Chain groupB -> Ordering)
    -> (item -> Array (Chain groupB))
    -> Builder subj groupA item
    -> Builder subj groupB item
regroupByMany ordFn itemToGroups = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) =
            Subject s
                $ backToGroups
                $ Array.groupExtBy ordFn Tuple.fst Tuple.snd
                $ allItemsOf groups

        allItemsOf :: Array (Group groupA item) -> Array (Chain groupB /\ Item item)
        allItemsOf = map (\(Group _ items) -> addGroups <$> items) >>> Array.concatMap concatF

        concatF :: Array (Item item /\ Array (Chain groupB)) -> Array (Chain groupB /\ Item item)
        concatF = map (\(item /\ chains) -> (flip (/\) item) <$> chains) >>> Array.concat

        addGroups (Item i) = Item i /\ itemToGroups i

        backToGroups :: Array (Chain groupB /\ Array (Item item)) -> Array (Group groupB item)
        backToGroups = map \(groupC /\ items) -> Group groupC items


allGroups :: forall subj group item. Builder subj group item -> Array group
allGroups = allGroupsC >>> map Chain.last


allGroupsC :: forall subj group item. Builder subj group item -> Array (Chain group)
allGroupsC = unwrap >>> map extractGroups >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group groupC _) = groupC


allGroupsOf :: forall subj_id subj group item. Eq subj_id => RC.IsSubjectId subj_id subj => subj_id -> Builder subj group item -> Array group
allGroupsOf subjId = allGroupsOfC subjId >>> map Chain.last


allGroupsOfC :: forall subj_id subj group item. Eq subj_id => RC.IsSubjectId subj_id subj => subj_id -> Builder subj group item -> Array (Chain group)
allGroupsOfC subjId = unwrap >>> map extractGroups >>> Array.concat
    where
        extractGroups (Subject otherSubj groups) | RC.s_id otherSubj == subjId = extractGroupC <$> groups
        extractGroups (Subject otherSubj groups) | otherwise = []
        extractGroupC (Group groupC _) = groupC


{- TODO
allGroupsWithItems :: forall subj group item. Builder subj group item -> Array (group /\ Array item)
allGroupsWithItems = allGroupsWithItemsC >>> map (lmap Chain.toArray) >>> ?wh
-}


allGroupsWithItemsC :: forall subj group item. Builder subj group item -> Array (Chain group /\ Array item)
allGroupsWithItemsC = unwrap >>> map extractGroups >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group groupC items) = groupC /\ (unwrap <$> items)


filterGroups :: forall subj group item. (subj -> Chain group -> Boolean) -> Builder subj group item -> Builder subj group item
filterGroups filterF = unwrap >>> map mapSubjectF >>> wrap
    where
        mapSubjectF (Subject s groups) = Subject s $ Array.filter (\(Group g _) -> filterF s g) groups


findGroup :: forall subj group item. (subj -> Chain group -> Boolean) -> Builder subj group item -> Maybe (Chain group)
findGroup findF = findMapGroup (\s gc -> if findF s gc then Just gc else Nothing)


findMapGroup :: forall subj group item a. (subj -> Chain group -> Maybe a) -> Builder subj group item -> Maybe a
findMapGroup findF = unwrap >>> Array.findMap findSubjectF
    where
        findSubjectF (Subject s groups) = Array.findMap (\(Group gc _) -> findF s gc) groups


{- Items -}


mapItems :: forall subj group itemA itemB. (itemA -> itemB) -> Builder subj group itemA -> Builder subj group itemB
mapItems = rmap


withItem :: forall subj group itemA itemB. (subj -> Chain group -> itemA -> itemB) -> Builder subj group itemA -> Builder subj group itemB
withItem imapF = unwrap >>> map mapSubjectF >>> wrap
    where
        mapSubjectF (Subject s groups) = Subject s $ mapGroupF s <$> groups
        mapGroupF s (Group gc items) = Group gc $ (unwrap >>> imapF s gc >>> wrap) <$> items


withItemIdx :: forall subj group itemA itemB. (subj -> Chain group -> Int -> itemA -> itemB) -> Builder subj group itemA -> Builder subj group itemB
withItemIdx imapF = unwrap >>> map mapSubjectF >>> wrap
    where
        mapSubjectF (Subject s groups) = Subject s $ mapGroupF s <$> groups
        mapGroupF s (Group gc items) = Group gc $ mapWithIndex (\idx -> wrap <<< imapF s gc idx <<< unwrap) items


sortItems :: forall subj group item. Ord item => Builder subj group item -> Builder subj group item
sortItems = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups)    = Subject s $ sortGroup <$> groups
        sortGroup (Group gc items) = Group gc $ Array.sort items


sortItemsWith :: forall subj group item a. Ord a => (item -> a) -> Builder subj group item -> Builder subj group item
sortItemsWith toA = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups)    = Subject s $ sortGroup <$> groups
        sortGroup (Group gc items) = Group gc $ Array.sortWith (unwrap >>> toA) items


sortItemsBy :: forall subj group item. (item -> item -> Ordering) -> Builder subj group item -> Builder subj group item
sortItemsBy = sortItemsByWith identity


sortItemsByWith :: forall subj group item a. (item -> a) -> (a -> a -> Ordering) -> Builder subj group item -> Builder subj group item
sortItemsByWith toA ordFn = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ sortGroup <$> groups
        sortGroup (Group gc items) = Group gc $ Array.sortBy sortByFn items
        sortByFn (Item iA) (Item iB) = ordFn (toA iA) (toA iB)


allItems :: forall subj group item. Builder subj group item -> Array item
allItems = unwrap >>> map extractGroups >>> map Array.concat >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group _ items) = unwrap <$> items


filterItems :: forall subj group item. (subj -> Chain group -> item -> Boolean) -> Builder subj group item -> Builder subj group item
filterItems filterF = unwrap >>> map mapSubjectF >>> wrap
    where
        mapSubjectF (Subject s groups) = Subject s $ mapGroupF s <$> groups
        mapGroupF s (Group gc items) = Group gc $ Array.filter (unwrap >>> filterF s gc) items


findItem :: forall subj group item. (subj -> Chain group -> item -> Boolean) -> Builder subj group item -> Maybe item
findItem findF = findMapItem \s gc item -> if findF s gc item then Just item else Nothing


findMapItem :: forall subj group item a. (subj -> Chain group -> item -> Maybe a) -> Builder subj group item -> Maybe a
findMapItem findF = unwrap >>> Array.findMap findSubjectF
    where
        findSubjectF (Subject s groups) = Array.findMap (\(Group gc items) -> Array.findMap (unwrap >>> findF s gc) items) groups


findMapItems :: forall subj group item a. (subj -> Chain group -> Array item -> Maybe a) -> Builder subj group item -> Maybe a
findMapItems findF = unwrap >>> Array.findMap findSubjectF
    where
        findSubjectF (Subject s groups) = Array.findMap (\(Group gc items) -> findF s gc $ unwrap <$> items) groups


directItemsOf :: forall subj group item. (subj -> Chain group -> Boolean) -> Builder subj group item -> Maybe (Array item)
directItemsOf predF = findMapItems $ \subj groupC items -> if predF subj groupC then Just items else Nothing


allSubjectsItems :: forall subj group item. (subj -> Boolean) -> Builder subj group item -> Maybe (Array (subj /\ Array (Chain group /\ Array item)))
allSubjectsItems subjPredF =
    unwrap
    >>> Array.filter (extractSubj >>> subjPredF)
    >>> case _ of
        [] -> Nothing
        arr -> Just $ extractGroups <$> arr
    >>> map (map $ map $ map extractGroup)
    where
        extractSubj (Subject s _) = s
        extractGroups (Subject s groups) = s /\ groups
        extractGroup (Group groupC items) = groupC /\ (unwrap <$> items)


allSubjectsItems_ :: forall subj group item. (subj -> Boolean) -> Builder subj group item -> Array item
allSubjectsItems_ subjPredF =
    allSubjectsItems subjPredF
    >>> fromMaybe []
    >>> map (Tuple.snd >>> map Tuple.snd)
    >>> map Array.concat
    >>> Array.concat


allGroupsItems :: forall subj group item. (subj -> Chain group -> Boolean) -> Builder subj group item -> Maybe (Array (subj /\ Array (Chain group /\ Array item)))
allGroupsItems groupPredF =
    unwrap
    >>> map extractGroups
    >>> map (\(subj /\ groups) -> subj /\ Array.filter (groupPredF subj <<< extractGroupC) groups)
    >>> case _ of
        [] -> Nothing
        arr -> Just $ (map $ map extractGroup) <$> arr
    where
        extractGroups (Subject s groups) = s /\ groups
        extractGroupC (Group groupC _) = groupC
        extractGroup (Group groupC items) = groupC /\ (unwrap <$> items)


allGroupsItems_ :: forall subj group item. (subj -> Chain group -> Boolean) -> Builder subj group item -> Array item
allGroupsItems_ groupPredF =
    allGroupsItems groupPredF
    >>> fromMaybe []
    >>> map (Tuple.snd >>> map Tuple.snd)
    >>> map Array.concat
    >>> Array.concat


