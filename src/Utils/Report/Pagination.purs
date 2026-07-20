module Report.Utils.Pagination where

import Prelude

import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array.NonEmpty (length) as NEA
-- import Data.Text.Format as F

import Report.Utils.Pages as P


newtype Pagination idx = Pagination (Array { index :: idx, count :: Int })
derive instance Newtype (Pagination idx) _


make :: forall idx item. P.Pages idx item -> Pagination idx
make = P.extract >>> map loadData >>> Pagination
    where loadData (idx /\ items) = { index : idx, count : NEA.length items }