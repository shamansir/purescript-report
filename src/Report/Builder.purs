module Report.Builder
    ( Builder(..)
    , Subject(..)
    , Group(..)
    , Item(..)
    , TreeNode(..)
    , empty
    , build
    , toBuilder, toBuilderC
    , unfold, unfoldC
    , toTree
    , fromTree -- FIXME: close this
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
    , withGroup, withGroupIdx
    , findGroup, findMapGroup
    , sortGroups, sortGroupsWith, sortGroupsBy
    , regroup, regroupBy, regroupByMany
    {- Items -}
    , mapItems
    , allItems
    , filterItems
    , withItem, withItemIdx
    , sortItems, sortItemsWith, sortItemsBy, sortItemsByWith
    , findItem, findMapItem, findMapItem'
    ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (concat, catMaybes, sort, sortWith, sortBy, groupAll, groupAllBy, filter, find, findMap, concatMap) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, toArray, fromArray) as NEA
import Data.Array.Extra (groupExt, groupExtBy) as Array
import Data.String (joinWith) as String
import Data.FunctorWithIndex (mapWithIndex)
import Data.Bifunctor (class Bifunctor, rmap, lmap)
import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Chain (Chain)
import Report.Chain as Chain

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, leaf, break, build) as Tree


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


unfold :: forall subj group item. Builder subj group item -> Array (subj /\ Array (group /\ Array item))
unfold = unfoldC >>> map (map $ foldl unfoldF [])
    where
        unfoldF prev (gchain /\ items) = prev <> (flip (/\) [] <$> Chain.beforeLast gchain) <> [ Chain.last gchain /\ items ]


unfoldC :: forall subj group item. Builder subj group item -> Array (subj /\ Array (Chain group /\ Array item))
unfoldC = unwrap >>> map \(Subject subj groups) -> subj /\ (unfoldGroup <$> groups)
    where
        unfoldGroup (Group groupC items) = groupC /\ (unwrap <$> items)


toTree :: forall subj group item. Builder subj group item -> Tree (TreeNode subj group item)
toTree (Builder subjects) =
    Tree.node NRoot $ toSubjectTree <$> subjects
    where
        toSubjectTree :: Subject subj group item -> Tree (TreeNode subj group item)
        toSubjectTree (Subject subj groupsArr) =
            Tree.node (NSubject subj) $ toGroupTree <$> groupsArr
        toGroupTree :: Group group item -> Tree (TreeNode subj group item)
        toGroupTree goi =
            case goi of
                Group groupC goiArr ->
                    foldChain groupC goiArr
        foldChain :: Chain group -> Array (Item item) -> Tree (TreeNode subj group item)
        foldChain groupC subGoiArr =
            case groupC of
                Chain.End g ->
                    Tree.node (NGroup g) $ Tree.leaf <$> (\(Item i) -> NItem i) <$> subGoiArr
                Chain.More g restC ->
                    Tree.node (NGroup g) $ pure $ foldChain restC subGoiArr


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


{- Groups -}


mapGroups :: forall subj groupA groupB item. (groupA -> groupB) -> Builder subj groupA item -> Builder subj groupB item
mapGroups = lmap


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
            Subject s $ backToGroups $ Array.groupExtBy ordFn Tuple.fst Tuple.snd $ allItemsOf groups

        allItemsOf :: Array (Group groupA item) -> Array (Chain groupB /\ Item item)
        allItemsOf = map (\(Group _ items) -> addGroups <$> items) >>> Array.concatMap concatF

        concatF :: Array (Item item /\ Array (Chain groupB)) -> Array (Chain groupB /\ Item item)
        concatF = map (\(item /\ chains) -> (flip (/\) item) <$> chains) >>> Array.concat

        addGroups (Item i) = Item i /\ itemToGroups i

        backToGroups :: Array (Chain groupB /\ Array (Item item)) -> Array (Group groupB item)
        backToGroups = map \(groupC /\ items) -> Group groupC items


allGroups :: forall subj group item. Builder subj group item -> Array group
allGroups = allGroupsC >>> map Chain.toArray >>> Array.concat


allGroupsC :: forall subj group item. Builder subj group item -> Array (Chain group)
allGroupsC = unwrap >>> map extractGroups >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group groupC _) = groupC


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


findMapItem' :: forall subj group item a. (subj -> Chain group -> Array item -> Maybe a) -> Builder subj group item -> Maybe a
findMapItem' findF = unwrap >>> Array.findMap findSubjectF
    where
        findSubjectF (Subject s groups) = Array.findMap (\(Group gc items) -> findF s gc $ unwrap <$> items) groups