module Report.Convert.Types where

import Prelude

import Foreign (Foreign)
import Foreign (ForeignError, renderForeignError) as F

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty (toUnfoldable) as NEL
import Data.String (joinWith) as String

import Yoga.JSON (class WriteForeign, class ReadForeign)

import StringParser (ParseError, printParserError) as SP

import Report (Report)
import Report.Class
import Report.Core (ReportFormat)
import Report.Group (Group)
import Report.Chain (Chain)
import Report.Chain (Chain(..)) as C
import Report.Decorators.Stats (Stats)
import Report.Decorators.Stats (Stats(..)) as ST
import Report.Tabular as Tabular
import Report.Tabular (Tabular)
import Report.Impl.Subject (Subject(..)) as Impl
import Report.Impl.Item (Item) as Impl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Tag (Tag) as Impl
import Report.Decorators.Tags (RawTag, RawTags)
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Convert.Keyed (class EncodableKey)


type SubjectRec =
    { name :: String
    , id :: SubjectId
    , stats :: Stats
    -- , trackedAt :: Maybe DateRec -- TODO
    -- , properties :: Array DecoratorRec
    , tags :: Array RawTag
    , tabulars :: Array TabularRec
    }


type ItemRec =
    { title :: String
    , decorators :: Array DecoratorRec
    , tabulars :: Array TabularRec
    , tags :: RawTags
    }



type DecoratorRec =
    { mkey :: String
    , fvalue :: Foreign
    }


type TabularRec =
    { tkey :: String
    , tlabel :: String
    , value :: TabularValue
    }


newtype SubjectId = SubjectId String
derive instance Newtype SubjectId _
derive newtype instance Eq SubjectId
derive newtype instance ConvertFrom String SubjectId
derive newtype instance ConvertTo String SubjectId
derive newtype instance ReadForeign SubjectId
derive newtype instance WriteForeign SubjectId
derive newtype instance EncodableKey SubjectId


newtype Subject = Subject SubjectRec
derive instance Newtype Subject _

--derive newtype instance ReadForeign Subject
derive newtype instance WriteForeign Subject
derive newtype instance ReadForeign Subject


newtype SubjectWithGroups = SubjectWithGroups { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } }
derive instance Newtype SubjectWithGroups _
derive newtype instance WriteForeign SubjectWithGroups
derive newtype instance ReadForeign  SubjectWithGroups


newtype ExportVersion = ExportVersion Int
derive newtype instance WriteForeign ExportVersion
derive newtype instance ReadForeign  ExportVersion


newtype ImportVersion = ImportVersion Int
derive newtype instance WriteForeign ImportVersion
derive newtype instance ReadForeign  ImportVersion


newtype ReportToExport = ReportToExport { version :: ExportVersion, subjects :: Array SubjectWithGroups }
derive instance Newtype ReportToExport _
derive newtype instance WriteForeign ReportToExport


newtype ReportToImport = ReportToImport { version :: ImportVersion, subjects :: Array SubjectWithGroups }
derive instance Newtype ReportToImport _
derive newtype instance ReadForeign ReportToImport


data Input
    = FileInput String
    | SampleIn {- which -}
    | StdInput


data Output
    = Screen
    | StdOutput
    | FileOutput String
    | NullOutput


data ImportError
    = FromParser SP.ParseError
    | FromJson (NonEmptyList F.ForeignError)
    | FailedToReadInput Input
    | UnsupportedFormat ReportFormat
    -- | ImportError String


printImportError :: ImportError -> String
printImportError = case _ of
    FromParser sperr -> SP.printParserError sperr
    FromJson ferr -> String.joinWith "; " $ F.renderForeignError <$> NEL.toUnfoldable ferr
    FailedToReadInput input -> "Failed to read input: " <> case input of
        FileInput filePath -> "File: " <> filePath
        SampleIn -> "Sample Input"
        StdInput -> "Standard Input"
    UnsupportedFormat fmt -> "Unsupported format: " <> show fmt
    -- ImportError ierr -> ierr


type RawReport' subj_id = Report (Impl.Subject subj_id Impl.Tag) Impl.Group (Impl.Item Impl.Tag)
type RawReport = RawReport' SubjectId


data UnitSubject = US
derive instance Eq UnitSubject
instance Show UnitSubject where show = const "subj"
instance IsSubjectId String  UnitSubject where s_id      = const "subj"
instance IsSubject   String  UnitSubject where s_name    = const "Subject"
instance HasTags     UnitTag UnitSubject where i_tags    = const []
instance HasTabular          UnitSubject where i_tabular = const Tabular.empty
instance HasStats            UnitSubject where i_stats   = const ST.SNotRelevant


data UnitTag = UT
instance ConvertTo (Chain String)   UnitTag where convertTo   = const $ C.End "subj"
instance ConvertFrom (Chain String) UnitTag where convertFrom = const $ Just UT
instance IsTag UnitTag where
    tagContent = const $ C.End "Tag"
    tagColors = const defaultTagColors


