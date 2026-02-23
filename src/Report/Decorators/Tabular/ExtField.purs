module Report.Decorators.Tabular.ExtField where

import Data.Tuple.Nested (type (/\))

import Report.Tabular (Tabular)

-- reimplements Input.Library.Item.Field


data ExtField v
    = LIField v
    -- | LISectionTitle v -- an internal hack
    | LIValues (Array v)
    | LIValuesNest (Array (Array v))
    | LITabulars (Array (Tabular (ExtField v)))
    | LITabularsNest
        { direct :: Array (Tabular (ExtField v))
        , parts :: Array (v /\ Array (Tabular (ExtField v)))
        }


