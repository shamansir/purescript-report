module Report.Utils.Pagination.LocatedPage where

import Prelude

import Data.Newtype (wrap, unwrap)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (length, toArray) as NEA
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (class Bifunctor, lmap)

import Report.Utils.Pages as P
import Report.Utils.Pagination
import Report.Utils.Pagination (make) as Pagination
import Report.Utils.Pagination.At (At)
import Report.Utils.Pagination.At as PosAt


newtype LocatedPage idx item
    = LocatedPage
        { pagination :: Pagination (At idx)
        , index :: idx
        , items :: NonEmptyArray item
        }


derive instance Functor (LocatedPage idx)
derive instance Bifunctor LocatedPage


locate :: forall idx item. P.Pages idx item -> P.Pages idx (LocatedPage idx item)
locate thePages =
    thePages
        # P.withItems (curry $ pure <<< toLocatedPage)
    where
        thePagination = Pagination.make thePages # PosAt.fill
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }


locateToIndex :: forall idx item. P.Pages idx item -> P.Pages (LocatedPage idx Unit) item
locateToIndex thePages =
    thePages
        # P.morphWith (\idx theItems -> toLocatedPage (idx /\ pure unit) /\ theItems)
    where
        thePagination = Pagination.make thePages # PosAt.fill
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }



pages :: forall idx item. P.Pages idx item -> Array (LocatedPage idx item)
pages thePages =
    unwrap thePages # map (unwrap >>> toLocatedPage)
    where
        thePagination = Pagination.make thePages # PosAt.fill
        toLocatedPage (index /\ items) = LocatedPage { pagination : thePagination, index, items }


index :: forall idx item. LocatedPage idx item -> idx
index (LocatedPage { index }) = index


items :: forall idx item. LocatedPage idx item -> NonEmptyArray item
items (LocatedPage { items }) = items



