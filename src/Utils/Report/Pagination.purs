module Report.Utils.Pagination where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (length, toArray) as NEA
-- import Data.Text.Format as F

import Report.Utils.Pages as P


newtype Pagination idx = Pagination (Array { index :: idx, count :: Int })
derive instance Newtype (Pagination idx) _


newtype LocatedPage idx item
    = LocatedPage
        { pagination :: Pagination idx
        , index :: idx
        , items :: NonEmptyArray item
        }


{-
data PageLoc idx
    = Before idx
    | Current -- idx
    | After idx
-}


locPages :: forall idx item. P.Pages idx item -> Array (LocatedPage idx item)
locPages thePages =
    unwrap thePages # map (unwrap >>> toLocatedPage)
    where
        thePagination = pagination thePages
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }


locPageIndex :: forall idx item. LocatedPage idx item -> idx
locPageIndex (LocatedPage { index }) = index


pagination :: forall idx item. P.Pages idx item -> Pagination idx
pagination = P.extract >>> map loadData >>> Pagination
    where loadData (idx /\ items) = { index : idx, count : NEA.length items }