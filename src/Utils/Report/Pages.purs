module Report.Utils.Pages where

import Prelude

import Data.Maybe (Maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (fst, snd) as Tuple
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either)

import Report.Utils.Grouping (Group, Grouping)
import Report.Utils.Grouping as Grouping


import Yoga.Tree (Tree)


type Pages idx item = Grouping idx item


type Page idx item = Group idx item


distribute :: forall item. Int -> Array item -> Pages Int item
distribute = Grouping.distribute


groupBy :: forall idx item. Ord idx => (item -> idx) -> Array item -> Pages idx item
groupBy = Grouping.groupBy


groupWithIndex :: forall idx item. Ord idx => (Int -> item -> idx) -> Array item -> Pages idx item
groupWithIndex = Grouping.groupWithIndex


pages :: forall idx item. Pages idx item -> Array (Page idx item)
pages = Grouping.groups


pageIndex :: forall idx item. Page idx item -> idx
pageIndex = Grouping.groupIndex


pageItems :: forall idx item. Page idx item -> NonEmptyArray item
pageItems = Grouping.groupItems


extract :: forall idx item. Pages idx item -> Array (idx /\ NonEmptyArray item)
extract = Grouping.extract


flatten :: forall idx item. Pages idx item -> Array (idx /\ item)
flatten = Grouping.flatten


make :: forall idx item. Ord idx => Array (idx /\ item) -> Pages idx item
make = Grouping.make


indices :: forall idx item. Pages idx item -> Array idx
indices = extract >>> map Tuple.fst


mergeItems :: forall idx item. Pages idx item -> Array item
mergeItems = Grouping.mergeItems


injectIndex :: forall idx item. Pages idx item -> Pages idx (idx /\ item)
injectIndex = Grouping.injectIndex


withItems :: forall idx itemA itemB. (idx -> NonEmptyArray itemA -> NonEmptyArray itemB) -> Pages idx itemA -> Pages idx itemB
withItems = Grouping.withItems


switchHeaders :: forall a b c. (a -> c) -> Pages a b -> Pages c b
switchHeaders = Grouping.switchIndices


empty :: forall a b. Pages a b
empty = Grouping.empty


singleton :: forall b. NonEmptyArray b -> Pages Unit b
singleton = Grouping.singleton


find :: forall idx a. Eq idx => idx -> Pages idx a -> Maybe (Page idx a)
find = Grouping.find


catMaybes :: forall i a. Pages i (Maybe a) -> Pages i a
catMaybes = Grouping.catMaybes


nest :: forall i ai b. Eq ai => Pages i ai -> Pages ai b -> Pages i (Page ai b)
nest = Grouping.nest


split :: forall i a bi b. Eq bi => (a -> bi /\ Array b) -> Pages i a -> Pages i (Page bi b)
split = Grouping.split


splitWithIndex :: forall i a bi b. Eq bi => (i -> a -> bi /\ Array b) -> Pages i a -> Pages i (Page bi b)
splitWithIndex = Grouping.splitWithIndex



pageToTree :: forall idx a c. (idx -> c) -> (a -> c) -> Page idx a -> Tree c
pageToTree = Grouping.groupToTree


pageToTree' :: forall idx a. Page idx a -> Tree (Either idx a)
pageToTree' = Grouping.groupToTree'


toTree :: forall idx a c. (idx -> c) -> (a -> c) -> c -> Pages idx a -> Tree c
toTree = Grouping.toTree


toTree' :: forall idx a. a -> Pages idx a -> Tree (Either idx a)
toTree' = Grouping.toTree'

