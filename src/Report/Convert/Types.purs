module Report.Convert.Types where

import Prelude

import Foreign (Foreign, fail)
import Foreign (fail, ForeignError(..)) as F

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class WriteForeign, writeImpl, class ReadForeign, readImpl)

import Report.Group (Group)
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Progress (DateRec)
import Report.Tabular (Tabular)
import Report.Modifiers.Tabular.TabularValue (TabularValue)


data MKind
    = P -- Prefix
    | S -- Suffix
    -- | X -- Unknown


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
derive newtype instance ReadForeign SubjectId
derive newtype instance WriteForeign SubjectId


type SubjectRec =
    { name :: String
    , id :: SubjectId
    , stats :: Stats
    -- , trackedAt :: Maybe DateRec -- TODO
    -- , properties :: Array ModifierRec
    , tags :: Array String
    , tabular :: Tabular TabularValue
    }


newtype Subject = Subject SubjectRec
derive instance Newtype Subject _

--derive newtype instance ReadForeign Subject
derive newtype instance WriteForeign Subject


newtype SubjectWithGroups = SubjectWithGroups { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } }
derive instance Newtype SubjectWithGroups _
derive newtype instance WriteForeign SubjectWithGroups


newtype ExportVersion = ExportVersion Int
derive newtype instance WriteForeign ExportVersion


newtype ReportToExport = ReportToExport { version :: ExportVersion, subjects :: Array SubjectWithGroups }
derive instance Newtype ReportToExport _
derive newtype instance WriteForeign ReportToExport


instance ReadForeign MKind where
    readImpl frgn = do
        str <- readImpl frgn
        case str of
            "P" -> pure P
            "S" -> pure S
            _   -> fail $ F.ForeignError $ "Unknown MKind: " <> str


instance WriteForeign MKind where
    writeImpl P = writeImpl "P"
    writeImpl S = writeImpl "S"