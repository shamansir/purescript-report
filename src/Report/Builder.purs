module Report.Builder where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (catMaybes) as Array

import Report.Chain (Chain)
import Report.Chain as Chain

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (node, leaf, break, build) as Tree


newtype Builder s g i = Builder (Array (Subject s g i))
data Subject s g i = Subject s (Array (Group g i))
data Group g i = Group (Chain g) (Array (Item i))
newtype Item i = Item i


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


toBuilder :: forall subj group item. Array (subj /\ Array (group /\ Array item)) -> Builder subj group item
toBuilder subjects =
    Builder $ toSubjectR <$> subjects
    where
        toSubjectR :: subj /\ Array (group /\ Array item) -> Subject subj group item
        toSubjectR (subj /\ groupsArr) =
            Subject subj $ toGroupR <$> groupsArr
        toGroupR :: group /\ Array item -> Group group item
        toGroupR (group /\ itemsArr) =
            Group (Chain.singleton group) (Item <$> itemsArr)


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