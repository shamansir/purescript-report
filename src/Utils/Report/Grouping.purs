module Report.Utils.Grouping where

import Prelude

import Debug as Debug


import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (fst, snd, uncurry, curry) as Tuple
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (head, toArray, length, fromArray, catMaybes) as NEA
import Data.Array.Extra as ArrayExt
import Data.Bifunctor (class Bifunctor, bimap, lmap)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Foldable (foldl)

import Data.Either (Either(..))

import Control.Comonad.Cofree (buildCofree)


import Yoga.Tree (Tree)
-- import Yoga.Tree as Tree
import Yoga.Tree.Extended (node, leaf, value, children, break) as Tree
import Yoga.Tree.Extended.Convert (toString, showTree') as Tree

import Report (Report)
import Report (build) as Report


newtype Group idx item = Group (idx /\ NonEmptyArray item)
derive instance Newtype (Group idx item) _


newtype Grouping idx item = Grouping (Array (Group idx item))
derive instance Newtype (Grouping idx item) _


instance Functor (Group idx) where
    map f (Group arr) = Group $ map f <$> arr


instance Bifunctor Group where
    bimap f g (Group arr) = Group $ bimap f (map g) $ arr


instance Functor (Grouping idx) where
    map f (Grouping arr) = Grouping $ map f <$> arr

instance FunctorWithIndex (idx /\ Int) (Grouping idx) where
    mapWithIndex f (Grouping arr)
        = Grouping $ mapF <$> arr
        where
            mapF (Group (idx /\ items)) = Group $ idx /\ mapWithIndex ((idx /\ _) >>> f) items
-- else instance FunctorWithIndex idx (Pages idx)
--     where mapWithIndex f = mapWithIndex $ Tuple.fst >>> f


instance Bifunctor Grouping where
    bimap f g (Grouping arr) = Grouping $ bimap f g <$> arr


distribute :: forall item. Int -> Array item -> Grouping Int item
distribute groupSize =
    groupWithIndex \index _ -> index / groupSize


groupBy :: forall idx item. Ord idx => (item -> idx) -> Array item -> Grouping idx item
groupBy = groupWithIndex <<< const


groupWithIndex :: forall idx item. Ord idx => (Int -> item -> idx) -> Array item -> Grouping idx item
groupWithIndex toIndex =
    mapWithIndex (\i item -> toIndex i item /\ item) >>> Array.groupAllBy compareIndices >>> map (addIndex >>> Group) >>> Grouping
    where
        addIndex nea = Tuple.fst (NEA.head nea) /\ map Tuple.snd nea
        compareIndices itemA itemB = compare (Tuple.fst itemA) (Tuple.fst itemB)


groups :: forall idx item. Grouping idx item -> Array (Group idx item)
groups = unwrap


groupIndex :: forall idx item. Group idx item -> idx
groupIndex = unwrap >>> Tuple.fst


groupItems :: forall idx item. Group idx item -> NonEmptyArray item
groupItems = unwrap >>> Tuple.snd


extract :: forall idx item. Grouping idx item -> Array (idx /\ NonEmptyArray item)
extract = groups >>> map unwrap


flatten :: forall idx item. Grouping idx item -> Array (idx /\ item)
flatten = groups >>> map unwrap >>> map (\(a /\ bs) -> (/\) a <$> NEA.toArray bs) >>> Array.concat


make :: forall idx item. Ord idx => Array (idx /\ item) -> Grouping idx item
make = Grouping <<< map Group <<< map (\abs -> Tuple.fst (NEA.head abs) /\ (Tuple.snd <$> abs)) <<< Array.groupAllBy (\pa pb -> compare (Tuple.fst pa) (Tuple.fst pb))


indices :: forall idx item. Grouping idx item -> Array idx
indices = extract >>> map Tuple.fst


mergeItems :: forall idx item. Grouping idx item -> Array item
mergeItems = groups >>> map (unwrap >>> Tuple.snd) >>> foldl (\arr nearr -> NEA.toArray nearr <> arr) []


injectIndex :: forall idx item. Grouping idx item -> Grouping idx (idx /\ item)
injectIndex = withItems \a bs -> (/\) a <$> bs


withItems :: forall idx itemA itemB. (idx -> NonEmptyArray itemA -> NonEmptyArray itemB) -> Grouping idx itemA -> Grouping idx itemB
withItems f = groups >>> map unwrap >>> map (\(a /\ bs) -> a /\ f a bs) >>> map Group >>> Grouping


switchIndices :: forall a b c. (a -> c) -> Grouping a b -> Grouping c b
switchIndices f = groups >>> map (bimap f identity) >>> Grouping


empty :: forall a b. Grouping a b
empty = Grouping []


singleton :: forall b. NonEmptyArray b -> Grouping Unit b
singleton items = Grouping [ Group $ unit /\ items ]


find :: forall idx a. Eq idx => idx -> Grouping idx a -> Maybe (Group idx a)
find idx = extract >>> Array.find (Tuple.fst >>> (_ == idx)) >>> map Group


catMaybes :: forall i a. Grouping i (Maybe a) -> Grouping i a
catMaybes (Grouping arr) = Grouping $ Array.catMaybes $ checkPage <$> arr
    where
        checkPage (Group (idx /\ nea)) =
            nea
                # NEA.catMaybes
                # NEA.fromArray
                >>= (Just <<< Group <<< (idx /\ _))


nest :: forall i ai b. Eq ai => Grouping i ai -> Grouping ai b -> Grouping i (Group ai b)
nest aitems bitems = mapF <$> aitems # catMaybes
    where mapF idx = bitems # find idx


split :: forall i a bi b. Eq bi => (a -> bi /\ Array b) -> Grouping i a -> Grouping i (Group bi b)
split f aitems = mapF <$> aitems # catMaybes
    where
        mapF = f >>> map NEA.fromArray >>> case _ of
            (bi /\ Just nea) -> Just $ Group $ bi /\ nea
            (_  /\ Nothing) -> Nothing


regroup :: forall i a bi. Ord i => Ord bi => (a -> bi) -> Grouping i a -> Grouping i (Grouping bi a)
regroup = regroupWithIndex <<< const


regroupWithIndex :: forall i a bi. Ord i => Ord bi => (i -> a -> bi) -> Grouping i a -> Grouping i (Grouping bi a)
regroupWithIndex f = unwrap >>> map unwrap >>> regroupF >>> groupBy Tuple.fst >>> map Tuple.snd >>> map make
    where
        regroupF :: Array (i /\ NonEmptyArray a) -> Array (i /\ Array (bi /\ a))
        regroupF = map (map NEA.toArray) >>> map mapF
        mapF :: i /\ Array a -> i /\ Array (bi /\ a)
        mapF (i /\ as) = i /\ ((\a -> f i a /\ a) <$> as)


splitWithIndex :: forall i a bi b. Eq bi => (i -> a -> bi /\ Array b) -> Grouping i a -> Grouping i (Group bi b)
splitWithIndex f aitems = mapWithIndex (Tuple.fst >>> mapF) aitems # catMaybes
    where
        mapF idx = f idx >>> map NEA.fromArray >>> case _ of
            (bi /\ Just nea) -> Just $ Group $ bi /\ nea
            (_  /\ Nothing) -> Nothing



groupToTree :: forall idx a c. (idx -> c) -> (a -> c) -> Group idx a -> Tree c
groupToTree idxToC = groupToTreeWithIndex idxToC <<< const


groupToTreeWithIndex :: forall idx a c. (idx -> c) -> (idx -> a -> c) -> Group idx a -> Tree c
groupToTreeWithIndex idxToC aIdxToC (Group (idx /\ items)) = Tree.node (idxToC idx) $ NEA.toArray $ (Tree.leaf <<< aIdxToC idx) <$> items


groupToTree' :: forall idx a. Group idx a -> Tree (Either idx a)
groupToTree' = groupToTree Left Right


groupsTrees :: forall idx a c. (idx -> c) -> (a -> c) -> Grouping idx a -> Array (Tree c)
groupsTrees idxToC = groupsTreesWithIndex idxToC <<< const


groupsTreesWithIndex :: forall idx a c. (idx -> c) -> (idx -> a -> c) -> Grouping idx a -> Array (Tree c)
groupsTreesWithIndex idxToC aIdxToC (Grouping theGroups) = groupToTreeWithIndex idxToC aIdxToC <$> theGroups


groupsTrees' :: forall idx a. Grouping idx a -> Array (Tree (Either idx a))
groupsTrees' = groupsTrees Left Right


toTree :: forall idx a c. (idx -> c) -> (a -> c) -> c -> Grouping idx a -> Tree c
toTree idxToC = toTreeWithIndex idxToC <<< const


toTree' :: forall idx a. a -> Grouping idx a -> Tree (Either idx a)
toTree' = toTree Left Right <<< Right


toTreeWithIndex :: forall idx a c. (idx -> c) -> (idx -> a -> c) -> c -> Grouping idx a -> Tree c
toTreeWithIndex idxToC aIdxToC root = Tree.node root <<< groupsTreesWithIndex idxToC aIdxToC


data Helper ia ib x
  = HRoot
  | IdxA ia
  -- | IdxB ib
  | HNode x
  -- | HTree (Tree x)
  | HBNode ib (Array (Tree x))
--   | HTree (Array (ib /\ Tree x))


 -- FIXME: it seems `Grouping idxa (Group idxb n)` (`Group` instead of `Grouping idxb n` as a nested element) is the proper type for representing subgroups, but I still don't get to the end why exactly...
toTree_ :: forall idxa idxb n c. Show c => (idxa -> c) -> (idxb -> c) -> (n -> c) -> c -> Grouping idxa (Grouping idxb n) -> Tree c
toTree_ idxAToC idxBToC nToC cRoot (Grouping topGroups) =
    Tree.node cRoot $ topLevel <$> topGroups
    where
        topLevel :: Group idxa (Grouping idxb n) -> Tree c
        topLevel (Group (idxA /\ subGroupings)) =
            Tree.node (idxAToC idxA) $ foldl foldSubF [] $ NEA.toArray subGroupings
        foldSubF :: Array (Tree c) -> Grouping idxb n -> Array (Tree c)
        foldSubF arr (Grouping subGroups) = arr <> (subLevel <$> subGroups)
        subLevel :: Group idxb n -> Tree c
        subLevel (Group (idxB /\ items)) =
            Tree.node (idxBToC idxB) $ (Tree.leaf <<< nToC) <$> NEA.toArray items


-- toReportS :: forall idxa idxb n c. Grouping idx a -> Report idx Unit a

toReportG :: forall idx a. Grouping idx a -> Report Unit idx a
toReportG = unwrap >>> map unwrapGroup >>> (pure <<< (/\) unit) >>> Report.build
    where
        unwrapGroup :: Group idx a -> idx /\ Array a
        unwrapGroup = unwrap >>> map NEA.toArray


toReportG' :: forall idxa idxb a. Grouping idxa (Grouping idxb a) -> Report idxa idxb a
toReportG' =
    unwrap >>> map (unwrapGroupL1 >>> map (map unwrap >>> Array.concat >>> map unwrapGroupL2)) >>> Report.build
    where
        unwrapGroupL1 :: Group idxa (Grouping idxb a) -> idxa /\ Array (Grouping idxb a)
        unwrapGroupL1 = unwrap >>> map NEA.toArray
        unwrapGroupL2 :: Group idxb a -> idxb /\ Array a
        unwrapGroupL2 = unwrap >>> map NEA.toArray


{- TODO: fromReport -}

fromArray :: forall idxa idxb a. Array (idxa /\ (Array (idxb /\ (Array a)))) -> Grouping idxa (Group idxb a)
fromArray = map toGroup >>> Array.catMaybes >>> map toGrouping >>> Array.catMaybes >>> Grouping
    where
        toGroup :: forall x z. x /\ Array z -> Maybe (Group x z)
        toGroup (_ /\ []) = Nothing
        toGroup (x /\ items) = NEA.fromArray items <#> \is -> Group $ x /\ is
        toGrouping :: Group idxa (idxb /\ Array a) -> Maybe (Group idxa (Group idxb a))
        toGrouping (Group (idxa /\ nea)) =
            let mbItems = NEA.catMaybes $ toGroup <$> nea
            in  NEA.fromArray mbItems <#> \nea' -> Group $ idxa /\ nea'


build :: forall idxa idxb a. Ord idxa => Ord idxb => (a -> idxa) -> (a -> idxb) -> Array a -> Grouping idxa (Group idxb a)
build aToIdxa aToIdxb = buildWithIndex (const aToIdxa) (const aToIdxb)


buildWithIndex :: forall idxa idxb a. Ord idxa => Ord idxb => (Int -> a -> idxa) -> (Int -> a -> idxb) -> Array a -> Grouping idxa (Group idxb a)
buildWithIndex aToIdxa aToIdxb = buildWithIndex' aToIdxa aToIdxb identity


buildWithIndex' :: forall idxa idxb x a. Ord idxa => Ord x => (Int -> a -> idxa) -> (Int -> a -> x) -> (x -> idxb) -> Array a -> Grouping idxa (Group idxb a)
buildWithIndex' aToIdxa indexToX xToIdxb =
    -- ArrayExt.groupExt aToIdxa identity
    mapWithIndex (\idx a -> aToIdxa idx a /\ a)
    >>> ArrayExt.groupExt Tuple.fst Tuple.snd
    >>> map (map applyGrouping)
    >>> fromArray
    where
        applyGrouping :: Array a -> Array (idxb /\ Array a)
        applyGrouping =
            mapWithIndex executeGF
            >>> ArrayExt.groupExt Tuple.fst Tuple.snd
            >>> map (lmap xToIdxb)
        executeGF :: Int -> a -> x /\ a
        executeGF n item = indexToX n item /\ item





{-
fromLibrary :: LI.GroupingBy -> Library -> LibraryTreeReportW
fromLibrary groupingBy =
    Library.items
        -- group subjects by generic key first
        >>> ArrayExt.groupExt (LI.genericKeyOf >>> fromMaybe LI.K_Else) identity
        -- then build groups using makeGroupingF, LI.Index
        -- TODO: `LI.SortKey` also supports tags and shelves, it was in `postRegroup` of `LT.group`
        -- FIXME: use `Report.Utils.Grouping` as mediator?
        -- FIXME: be able to regroup the report easily by itself
        >>> map (map applyGrouping)
        >>> ReportB.toBuilderC
        >>> Report.fromBuilder
        >>> LRTW
    where
        applyGrouping :: Array LibraryItem -> Array (Chain Report.Group /\ Array LibraryItem)
        applyGrouping =
            mapWithIndex (executeGF $ LI.makeGroupingF groupingBy)
            >>> ArrayExt.groupExt Tuple.fst Tuple.snd
            >>> map (lmap $ indexToGroup >>> fromMaybe emptyGroup)
        emptyGroup :: Chain Report.Group
        emptyGroup = C.End R.unknownGroup
        executeGF :: LI.GroupingF -> Int -> LibraryItem -> LI.Index /\ LibraryItem
        executeGF (LI.GroupingF func) n item = func n item /\ item
        indexToGroup :: LI.Index -> Maybe (Chain Report.Group)
        indexToGroup = case _ of
            LI.Lost -> Nothing
            LI.ByMaybeStr mbStr -> mbStr <#> (\str -> R.mkGroup [ R.ps str ] $ str) <#> C.End
            LI.ByInt n -> Just $ C.End $ R.mkGroup [ R.ps $ show n ] $ show n
            LI.ByUKey ukey -> R.t_group ukey
            LI.ByGKey ukey -> R.t_group ukey
-}