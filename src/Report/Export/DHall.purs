module Report.Export.DHall where

import Prelude

import Foreign (Foreign)

import Data.Maybe (Maybe(..))


import Report (Report)
import Report.Modifiers.Progress (DateRec)


type DHallItemRec =
    { key :: Maybe String
    , title :: Maybe String
    , kind :: Maybe String
    , ref :: Array String
    , detailed :: Maybe String
    , selfRef :: Maybe (Array String)
    , date :: Maybe DateRec
    , tags :: Maybe (Array String)
    , value ::
        Maybe
            { t :: String
            , v :: Foreign
            }
    }


toDhall :: forall subj group item. Report subj group item -> String
toDhall report =
    "-- Dhall representation of Report is not implemented yet.\n\
    \\n\
    \-- This is a placeholder.\n\
    \\n\
    \let Report = < Placeholder : {} >\n\
    \\n\
    \in  Report\n"