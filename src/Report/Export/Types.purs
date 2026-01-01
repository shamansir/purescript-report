module Report.Export.Types where

import Prelude

import Foreign (Foreign)

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)


import Report.Group (Group)
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Progress (DateRec)


{-
type ItemRec =
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
    -- TODO: , tabular :: Array ({ key :: String, label :: String, value :: Foreign })
    }

-}

type ItemRec =
    { name :: String
    , modifiers :: Array { mkey :: String, value :: Foreign }
    , mbTitle :: Maybe String
    , locked :: Boolean
    -- TODO: , tabular :: Array ({ key :: String, label :: String, value :: Foreign })
    }



newtype SubjectId = SubjectId String


type SubjectRec =
    { name :: String
    , id :: SubjectId
    , tags :: Array String
    , stats :: Stats
    , trackedAt :: Maybe DateRec
    , properties :: Array ItemRec
    }


newtype Subject =
    Subject SubjectRec


newtype SubjectWithGroups = SubjectWithGroups { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } }
derive instance Newtype SubjectWithGroups _


newtype ReportToExport = ReportToExport (Array SubjectWithGroups)
derive instance Newtype ReportToExport _