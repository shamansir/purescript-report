module Report.Utils.Pagination.At where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Foldable (foldl, foldr)
import Data.Array (snoc, cons) as Array

import Report.Utils.Pagination (Pagination)


type AtRec idx = { before :: Array idx, current :: idx, after :: Array idx }


newtype At idx = At (AtRec idx)
derive instance Newtype (At idx) _


type PaginationAt idx = Pagination (At idx)


data AtPos idx
    = Before idx
    | Current idx
    | After idx


derive instance Functor At
derive instance Functor AtPos


fill :: forall idx. Pagination idx -> Pagination (At idx)
fill = unwrap >>> foldl foldLeftF [] >>> foldr foldRightF [] >>> wrap
    where
        foldLeftF prev { count, index } =
            Array.snoc
                prev
                { count
                , index : At
                    { before : extractCurrent <$> _.index <$> prev
                    , current : index
                    , after : []
                    }
                }
        foldRightF { count, index : At { current, before} } prev =
            Array.cons
                { count
                , index : At
                    { before : before
                    , current : current
                    , after : extractCurrent <$> _.index <$> prev
                    }
                }
                prev
        extractCurrent = unwrap >>> _.current


position :: forall idx. Pagination (At idx) -> Pagination (Array (AtPos idx))
position = map position_


position_ :: forall idx. At idx -> Array (AtPos idx)
position_ (At { before, current, after }) = (Before <$> before) <> [ Current current ] <> (After <$> after)


current :: forall idx. At idx -> idx
current = unwrap >>> _.current


before :: forall idx. At idx -> Array idx
before = unwrap >>> _.before


after :: forall idx. At idx -> Array idx
after = unwrap >>> _.after