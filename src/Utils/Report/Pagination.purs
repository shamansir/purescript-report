module Report.Utils.Pagination where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Array (snoc) as Array
import Data.Array.NonEmpty (length) as NEA
-- import Data.Text.Format as F

import Report.Utils.Pages as P


type Cell idx = { index :: idx, count :: Int }


newtype Pagination idx = Pagination (Array (Cell idx))
derive instance Newtype (Pagination idx) _
derive instance Functor Pagination


make :: forall idx item. P.Pages idx item -> Pagination idx
make = P.extract >>> map loadData >>> Pagination
    where loadData (idx /\ items) = { index : idx, count : NEA.length items }


withOffset :: Pagination Int -> Pagination { pos :: Int, offset :: Int }
withOffset = unwrap >>> foldl foldF (0 /\ []) >>> Tuple.snd >>> wrap
    where
        foldF
            :: Int /\ Array (Cell { pos :: Int, offset :: Int })
            -> Cell Int
            -> Int /\ Array (Cell { pos :: Int, offset :: Int })
        foldF (prevOffset /\ prevPg) { count, index } =
            let nextOffset = prevOffset + count
            in nextOffset /\ Array.snoc prevPg { count, index : { pos : index, offset : prevOffset } }