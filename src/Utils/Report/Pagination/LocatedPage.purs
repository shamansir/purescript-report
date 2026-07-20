module Utils.Report.Pagination.LocatedPage where

import Prelude

import Data.Newtype (wrap, unwrap)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (length, toArray) as NEA
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))

import Report.Utils.Pages as P
import Report.Utils.Pagination
import Report.Utils.Pagination (make) as Pagination


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


morph :: forall idx item. P.Pages idx item -> P.Pages idx (LocatedPage idx item)
morph thePages =
    thePages
        # P.withItems (curry $ pure <<< toLocatedPage)
    where
        thePagination = Pagination.make thePages
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }


pages :: forall idx item. P.Pages idx item -> Array (LocatedPage idx item)
pages thePages =
    unwrap thePages # map (unwrap >>> toLocatedPage)
    where
        thePagination = Pagination.make thePages
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }


index :: forall idx item. LocatedPage idx item -> idx
index (LocatedPage { index }) = index


items :: forall idx item. LocatedPage idx item -> NonEmptyArray item
items (LocatedPage { items }) = items



