module Report.Export.Types where

import Prelude

import Foreign (Foreign)

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class WriteForeign, writeImpl)

import Report.Group (Group)
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Progress (DateRec)
import Report.Modifiers.Class.ValueModify


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

data MKind
    = P -- Prefix
    | S -- Suffix


type ModifierRec =
    { mkey :: String
    , mkind :: MKind
    , value :: Foreign
    }


type ItemRec =
    { name :: String
    , modifiers :: Array ModifierRec
    , mbTitle :: Maybe String
    , locked :: Boolean
    -- TODO: , tabular :: Array ({ key :: String, label :: String, value :: Foreign })
    }


newtype SubjectId = SubjectId String
derive instance Newtype SubjectId _
derive newtype instance WriteForeign SubjectId


type SubjectRec =
    { name :: String
    , id :: SubjectId
    , tags :: Array String
    , stats :: Stats
    , trackedAt :: Maybe DateRec
    , properties :: Array ItemRec
    }


newtype Subject = Subject SubjectRec
derive instance Newtype Subject _

derive newtype instance WriteForeign Subject


newtype SubjectWithGroups = SubjectWithGroups { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } }
derive instance Newtype SubjectWithGroups _
derive newtype instance WriteForeign SubjectWithGroups


newtype ExportVersion = ExportVersion Int
derive newtype instance WriteForeign ExportVersion


newtype ReportToExport = ReportToExport { version :: ExportVersion, subjects :: Array SubjectWithGroups }
derive instance Newtype ReportToExport _
derive newtype instance WriteForeign ReportToExport


instance WriteForeign MKind where
    writeImpl P = writeImpl "P"
    writeImpl S = writeImpl "S"