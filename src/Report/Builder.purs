module Report.Builder where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (concat, catMaybes, sort, sortWith, sortBy, groupAll, groupAllBy) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, toArray) as NEA
import Data.Array.Extra (groupExt, groupExtBy) as Array
import Data.String (joinWith) as String
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


unfold :: forall subj group item. Builder subj group item -> Array (subj /\ Array (Chain group /\ Array item))
unfold = unwrap >>> map \(Subject subj groups) -> subj /\ (unfoldGroup <$> groups)
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
sortSubjectsBy = sortSubjectsBy' identity


sortSubjectsBy' :: forall subj group item a. (subj -> a) -> (a -> a -> Ordering) -> Builder subj group item -> Builder subj group item
sortSubjectsBy' toA ordFn =unwrap >>> Array.sortBy sortByFn >>> wrap
    where
        sortByFn (Subject sA _) (Subject sB _) = ordFn (toA sA) (toA sB)


allSubjects :: forall subj group item. Builder subj group item -> Array subj
allSubjects = unwrap >>> map extractSubj
    where
        extractSubj (Subject s _) = s


{- Groups -}


mapGroups :: forall subj groupA groupB item. (groupA -> groupB) -> Builder subj groupA item -> Builder subj groupB item
mapGroups = lmap


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
sortGroupsBy = sortGroupsBy' identity


sortGroupsBy' :: forall subj group item a. (group -> a) -> (Chain a -> Chain a -> Ordering) -> Builder subj group item -> Builder subj group item
sortGroupsBy' toA ordFn = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ Array.sortBy sortByFn groups
        sortByFn (Group gchainA _) (Group gchainB _) = ordFn (toA <$> gchainA) (toA <$> gchainB)


regroup :: forall subj groupA groupB item. Ord groupB => (item -> Chain groupB) -> Builder subj groupA item -> Builder subj groupB item
regroup = regroupBy compare


regroupBy :: forall subj groupA groupB item. (Chain groupB -> Chain groupB -> Ordering) -> (item -> Chain groupB) -> Builder subj groupA item -> Builder subj groupB item
regroupBy ordFn itemToGroup = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ backToGroups $ Array.groupAllBy groupByFn $ allItemsOf groups
        allItemsOf = map (\(Group _ items) -> addGroup <$> items) >>> Array.concat
        addGroup (Item i) = itemToGroup i /\ Item i
        groupByFn (gchainA /\ _) (gchainB /\ _) = ordFn gchainA gchainB
        backToGroups :: Array (NonEmptyArray (Chain groupB /\ Item item)) -> Array (Group groupB item)
        backToGroups = map (\nea -> (Tuple.fst $ NEA.head nea) /\ (Tuple.snd <$> NEA.toArray nea)) >>> map (\(groupC /\ items) -> Group groupC items)


allGroups :: forall subj group item. Builder subj group item -> Array group
allGroups = unwrap >>> map extractGroups >>> map Array.concat >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group groupC _) = Chain.toArray groupC


{- Items -}


mapItems :: forall subj group itemA itemB. (itemA -> itemB) -> Builder subj group itemA -> Builder subj group itemB
mapItems = rmap


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
sortItemsBy = sortItemsBy' identity


sortItemsBy' :: forall subj group item a. (item -> a) -> (a -> a -> Ordering) -> Builder subj group item -> Builder subj group item
sortItemsBy' toA ordFn = unwrap >>> map mapFn >>> wrap
    where
        mapFn (Subject s groups) = Subject s $ sortGroup <$> groups
        sortGroup (Group gc items) = Group gc $ Array.sortBy sortByFn items
        sortByFn (Item iA) (Item iB) = ordFn (toA iA) (toA iB)


allItems :: forall subj group item. Builder subj group item -> Array item
allItems = unwrap >>> map extractGroups >>> map Array.concat >>> Array.concat
    where
        extractGroups (Subject _ groups) = extractGroupC <$> groups
        extractGroupC (Group _ items) = unwrap <$> items