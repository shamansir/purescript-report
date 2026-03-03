module Report.Convert.Types where

import Prelude

import Foreign (Foreign, fail)
import Foreign (fail, ForeignError(..)) as F

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class WriteForeign, writeImpl, class ReadForeign, readImpl)

import Report.Group (Group)
import Report.Chain (Chain)
import Report.Decorators.Stats (Stats)
import Report.Decorators.Progress (DateRec)
import Report.Tabular (Tabular)
import Report.Decorators.Tags (RawTag)
import Report.Decorators.Tabular.TabularValue (TabularValue)


type DecoratorRec =
    { mkey :: String
    , value :: Foreign
    }


type ItemRec =
    { title :: String
    , decorators :: Array DecoratorRec
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
    -- , properties :: Array DecoratorRec
    , tags :: Array RawTag
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