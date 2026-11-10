module Report.AtomicValue.Marker.Org where

{- Markers of the `.org` format: the names of the properties which store the decorators of the item:
   atomic values, levels and other decorators share the namespace, since all of them are the properties of the same heading,
   i.e. `:RAV_TOGETI: 3 10`, `:RLV_LEVELSE: 3 7`, `:RDC_RATING: 7.5`.

   - every name starts with `RAV_` (atomic values), `RLV_` (levels) or `RDC_` (decorators),
     so that the values never mix with the core org properties (`ID`, `PRIORITY`, `TAGS`, ...);
   - org property names are case-insensitive, so the names contain only upper-case letters, digits and `_`:
     this way the uniqueness checked by `Marker` is also the case-insensitive uniqueness. -}

import Prelude

import Data.Map (Map)
import Prim.Symbol (class Cons) as Symbol
import Prim.TypeError (class Fail, Text, Quote, Beside)

import Report.Core (ReportFormat(..)) as CT
import Report.Core.Marker
    ( MarkerFormat
    , class Marker, class ValidMarker, class IsMarkerFormat, class MarkerArgument
    , toMarker, allMarkers, markersMap
    )
import Report.AtomicValue.Tag (AtomicValueTag)
import Report.Decorator (Key) as D
import Report.Decorators.Progress (PValueTag, PTValueTag, _vtagFrom, _vtagTo) as P


foreign import data OrgProperty :: MarkerFormat


instance IsMarkerFormat OrgProperty where
    reflectFormat _ = CT.Org


instance (ReportPrefix marker, OrgMarkerChars marker marker) => ValidMarker OrgProperty marker


{- Atomic values -}


instance Marker OrgProperty "AVTNone"          "RAV_NONE"
instance Marker OrgProperty "AVTUnknown"       "RAV_UNKNOWN"
instance Marker OrgProperty "AVTPText"         "RAV_TEXTVAL"
instance Marker OrgProperty "AVTPNumber"       "RAV_NUMVAL"
instance Marker OrgProperty "AVTPInt"          "RAV_INTVAL"
instance Marker OrgProperty "AVTPBoolean"      "RAV_BOOLVAL"
instance Marker OrgProperty "AVTPYear"         "RAV_YEARVAL"
instance Marker OrgProperty "AVTPID"           "RAV_UIDVAL"
instance Marker OrgProperty "AVTToComplete"    "RAV_TOCOMPLETE"
instance Marker OrgProperty "AVTToGetI"        "RAV_TOGETI"
instance Marker OrgProperty "AVTToGetN"        "RAV_TOGETN"
instance Marker OrgProperty "AVTToGetIX"       "RAV_TOGETIX"
instance Marker OrgProperty "AVTToGetNX"       "RAV_TOGETNX"
instance Marker OrgProperty "AVTToGetMI"       "RAV_TOGETMI"
instance Marker OrgProperty "AVTToGetMN"       "RAV_TOGETMN"
instance Marker OrgProperty "AVTToGetMIX"      "RAV_TOGETMIX"
instance Marker OrgProperty "AVTToGetMNX"      "RAV_TOGETMNX"
instance Marker OrgProperty "AVTPercentI"      "RAV_PERCENTI"
instance Marker OrgProperty "AVTPercentN"      "RAV_PERCENTN"
instance Marker OrgProperty "AVTPercentSign"   "RAV_PERCENTX"
instance Marker OrgProperty "AVTMeasuredI"     "RAV_MESI"
instance Marker OrgProperty "AVTMeasuredN"     "RAV_MESN"
instance Marker OrgProperty "AVTMeasuredSign"  "RAV_MESX"
instance Marker OrgProperty "AVTPerI"          "RAV_PERI"
instance Marker OrgProperty "AVTPerN"          "RAV_PERN"
instance Marker OrgProperty "AVTOnDate"        "RAV_ONDATE"
instance Marker OrgProperty "AVTOnTime"        "RAV_ONTIME"
instance Marker OrgProperty "AVTOnDateTime"    "RAV_ONDATETIME"
instance Marker OrgProperty "AVTTimeRange"     "RAV_TIMERANGE"
instance Marker OrgProperty "AVTDateRange"     "RAV_DATERANGE"
instance Marker OrgProperty "AVTDateTimeRange" "RAV_DATETIMERANGE"
instance Marker OrgProperty "AVTRangeI"        "RAV_RANGEI"
instance Marker OrgProperty "AVTRangeN"        "RAV_RANGEN"
instance Marker OrgProperty "AVTRelTime"       "RAV_RELTIME"
instance Marker OrgProperty "AVTRelInt"        "RAV_RELINT"
instance Marker OrgProperty "AVTRelNum"        "RAV_RELNUM"
instance Marker OrgProperty "AVTPTask"         "RAV_PROCVAL"
instance Marker OrgProperty "AVTPPath"         "RAV_PATHVAL"
instance Marker OrgProperty "AVTTags"          "RAV_TAGSVAL"
instance Marker OrgProperty "AVTPRawTags"      "RAV_RAWTAGSVAL"
instance Marker OrgProperty "AVTColor"         "RAV_COLORVAL"
instance Marker OrgProperty "AVTPRating"       "RAV_RATINGVAL"
instance Marker OrgProperty "AVTPPriority"     "RAV_PRIORITYVAL"
instance Marker OrgProperty "AVTError"         "RAV_ERRORVAL"


{- Levels -}


instance Marker OrgProperty "PTLevelsI" "RLV_LEVELSI"
instance Marker OrgProperty "PTLevelsN" "RLV_LEVELSN"
instance Marker OrgProperty "PTLevelsO" "RLV_LEVELSO"
instance Marker OrgProperty "PTLevelsS" "RLV_LEVELSS"
instance Marker OrgProperty "PTLevelsE" "RLV_LEVELSE"
instance Marker OrgProperty "PTLevelsP" "RLV_LEVELSP"
instance Marker OrgProperty "PTLevelsC" "RLV_LEVELSC"


{- Decorators -}


instance Marker OrgProperty "KRating"        "RDC_RATING"
instance Marker OrgProperty "KPriority"      "RDC_PRIORITY"
instance Marker OrgProperty "KTask"          "RDC_TASK"
instance Marker OrgProperty "KEarnedAt"      "RDC_EARNEDAT"
instance Marker OrgProperty "KDescription"   "RDC_DESCRIPTION"
instance Marker OrgProperty "KReference"     "RDC_REFERENCE"
instance Marker OrgProperty "KJoinReference" "RDC_JOINREFERENCE"


-- | `KProgress` decorators have the markers of their values
instance MarkerArgument OrgProperty P.PValueTag where
    argToMarker _ = P._vtagFrom >>> toMarker @OrgProperty
    argAllMarkers _ = map P._vtagTo <$> allMarkers @OrgProperty


{- Conversion -}


toOrgMarker :: AtomicValueTag -> String
toOrgMarker = toMarker @OrgProperty


toOrgDecoratorMarker :: D.Key -> String
toOrgDecoratorMarker = toMarker @OrgProperty


-- | the names are upper-case, the property name should be converted to upper case before the lookup
atomicTagByOrgMarker :: Map String AtomicValueTag
atomicTagByOrgMarker = markersMap @OrgProperty


-- | the names are upper-case, the property name should be converted to upper case before the lookup
progressTagByOrgMarker :: Map String P.PTValueTag
progressTagByOrgMarker = markersMap @OrgProperty


-- | the names are upper-case, the property name should be converted to upper case before the lookup
decoratorKeyByOrgMarker :: Map String D.Key
decoratorKeyByOrgMarker = markersMap @OrgProperty


{- Prefix check -}


-- | The name of the property starts with `RAV_`, `RLV_` or `RDC_`
class ReportPrefix :: Symbol -> Constraint
class ReportPrefix marker

instance (Symbol.Cons h t marker, ReportPrefixR marker h t) => ReportPrefix marker


class ReportPrefixR :: Symbol -> Symbol -> Symbol -> Constraint
class ReportPrefixR marker head tail

instance (Symbol.Cons h t tail, ReportPrefixK marker h t) => ReportPrefixR marker "R" tail
else instance NoReportPrefix marker => ReportPrefixR marker head tail


-- | The kind of the property: `AV` (atomic value), `LV` (levels) or `DC` (decorator)
class ReportPrefixK :: Symbol -> Symbol -> Symbol -> Constraint
class ReportPrefixK marker head tail

instance (Symbol.Cons h t tail, ReportPrefixV marker h t) => ReportPrefixK marker "A" tail
else instance (Symbol.Cons h t tail, ReportPrefixV marker h t) => ReportPrefixK marker "L" tail
else instance (Symbol.Cons h t tail, ReportPrefixC marker h t) => ReportPrefixK marker "D" tail
else instance NoReportPrefix marker => ReportPrefixK marker head tail


class ReportPrefixV :: Symbol -> Symbol -> Symbol -> Constraint
class ReportPrefixV marker head tail

instance (Symbol.Cons h t tail, ReportPrefixU marker h t) => ReportPrefixV marker "V" tail
else instance NoReportPrefix marker => ReportPrefixV marker head tail


class ReportPrefixC :: Symbol -> Symbol -> Symbol -> Constraint
class ReportPrefixC marker head tail

instance (Symbol.Cons h t tail, ReportPrefixU marker h t) => ReportPrefixC marker "C" tail
else instance NoReportPrefix marker => ReportPrefixC marker head tail


class ReportPrefixU :: Symbol -> Symbol -> Symbol -> Constraint
class ReportPrefixU marker head tail

instance ReportPrefixU marker "_" tail
else instance NoReportPrefix marker => ReportPrefixU marker head tail


class NoReportPrefix :: Symbol -> Constraint
class NoReportPrefix marker

instance Fail (Beside (Text "`.org` property name should start with `RAV_`, `RLV_` or `RDC_`: ") (Quote marker)) => NoReportPrefix marker


{- Characters check -}


-- | The name of the property contains only upper-case letters, digits and `_`
class OrgMarkerChars :: Symbol -> Symbol -> Constraint
class OrgMarkerChars marker rest

instance OrgMarkerChars marker ""
else instance (Symbol.Cons h t rest, OrgMarkerChar marker h, OrgMarkerChars marker t) => OrgMarkerChars marker rest


class OrgMarkerChar :: Symbol -> Symbol -> Constraint
class OrgMarkerChar marker char

instance OrgMarkerChar marker "A"
else instance OrgMarkerChar marker "B"
else instance OrgMarkerChar marker "C"
else instance OrgMarkerChar marker "D"
else instance OrgMarkerChar marker "E"
else instance OrgMarkerChar marker "F"
else instance OrgMarkerChar marker "G"
else instance OrgMarkerChar marker "H"
else instance OrgMarkerChar marker "I"
else instance OrgMarkerChar marker "J"
else instance OrgMarkerChar marker "K"
else instance OrgMarkerChar marker "L"
else instance OrgMarkerChar marker "M"
else instance OrgMarkerChar marker "N"
else instance OrgMarkerChar marker "O"
else instance OrgMarkerChar marker "P"
else instance OrgMarkerChar marker "Q"
else instance OrgMarkerChar marker "R"
else instance OrgMarkerChar marker "S"
else instance OrgMarkerChar marker "T"
else instance OrgMarkerChar marker "U"
else instance OrgMarkerChar marker "V"
else instance OrgMarkerChar marker "W"
else instance OrgMarkerChar marker "X"
else instance OrgMarkerChar marker "Y"
else instance OrgMarkerChar marker "Z"
else instance OrgMarkerChar marker "0"
else instance OrgMarkerChar marker "1"
else instance OrgMarkerChar marker "2"
else instance OrgMarkerChar marker "3"
else instance OrgMarkerChar marker "4"
else instance OrgMarkerChar marker "5"
else instance OrgMarkerChar marker "6"
else instance OrgMarkerChar marker "7"
else instance OrgMarkerChar marker "8"
else instance OrgMarkerChar marker "9"
else instance OrgMarkerChar marker "_"
else instance Fail (Beside (Text "`.org` property name should contain only upper-case letters, digits and `_`: ") (Quote marker)) => OrgMarkerChar marker char
