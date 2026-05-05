module Report.Convert.Types where

import Prelude

import Foreign (Foreign, fail)
import Foreign (fail, ForeignError(..)) as F

import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)

import Yoga.JSON (class WriteForeign, writeImpl, class ReadForeign, readImpl)

import StringParser (ParseError, printParserError) as SP

import Report (Report)
import Report.Class (class ConvertFrom, class ConvertTo)
import Report.Group (Group)
import Report.Chain (Chain)
import Report.Decorators.Stats (Stats)
import Report.Decorators.Progress (DateRec)
import Report.Tabular (Tabular)
import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Subject (mapTags) as SubjImpl
import Report.Impl.Item (Item(..)) as Impl
import Report.Impl.Item (mapTags) as ItemImpl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Tag (Tag(..)) as Impl
import Report.Decorators.Tags (RawTag, RawTags)
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Convert.Keyed (class EncodableKey)


type DecoratorRec =
    { mkey :: String
    , fvalue :: Foreign
    }


type TabularRec =
    { tkey :: String
    , tlabel :: String
    , value :: TabularValue
    }


type ItemRec =
    { title :: String
    , decorators :: Array DecoratorRec
    , tabulars :: Array TabularRec
    , tags :: RawTags
    }


newtype SubjectId = SubjectId String
derive instance Newtype SubjectId _
derive newtype instance Eq SubjectId
derive newtype instance ConvertFrom String SubjectId
derive newtype instance ConvertTo String SubjectId
derive newtype instance ReadForeign SubjectId
derive newtype instance WriteForeign SubjectId
derive newtype instance EncodableKey SubjectId


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


data ImportError
    = FromParser SP.ParseError
    | ImportError String


printImportError :: ImportError -> String
printImportError = case _ of
    FromParser sperr -> SP.printParserError sperr
    ImportError ierr -> ierr


type RawReport' subj_id = Report (Impl.Subject subj_id Impl.Tag) Impl.Group (Impl.Item Impl.Tag)
type RawReport = RawReport' SubjectId