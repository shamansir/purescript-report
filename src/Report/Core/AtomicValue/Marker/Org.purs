module Report.AtomicValue.Marker.Org where

{- Markers of the `.org` format: the names of the properties which store the atomic values,
   i.e. `:RAV_TOGETI: 3 10`.

   - every name starts with `RAV_`, so that the values never mix with the core org properties (`ID`, `PRIORITY`, `TAGS`, ...)
     and with the properties for the decorators (`Rating`, `Task`, ...);
   - org property names are case-insensitive, so the names contain only upper-case letters, digits and `_`:
     this way the uniqueness checked by `Marker` is also the case-insensitive uniqueness. -}

import Data.Map (Map)
import Prim.Symbol (class Cons) as Symbol
import Prim.TypeError (class Fail, Text, Quote, Beside)

import Report.Core (ReportFormat(..)) as CT
import Report.Core.Marker (MarkerFormat, class Marker, class ValidMarker, class IsMarkerFormat, toMarker, markersMap)
import Report.AtomicValue.Tag (AtomicValueTag)


foreign import data OrgProperty :: MarkerFormat


instance IsMarkerFormat OrgProperty where
    reflectFormat _ = CT.Org


instance (RavPrefix marker, OrgMarkerChars marker marker) => ValidMarker OrgProperty marker


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


{- Conversion -}


toOrgMarker :: AtomicValueTag -> String
toOrgMarker = toMarker @OrgProperty


-- | the names are upper-case, the property name should be converted to upper case before the lookup
atomicTagByOrgMarker :: Map String AtomicValueTag
atomicTagByOrgMarker = markersMap @OrgProperty


{- Prefix check -}


-- | The name of the property starts with `RAV_`
class RavPrefix :: Symbol -> Constraint
class RavPrefix marker

instance (Symbol.Cons h t marker, RavPrefixR marker h t) => RavPrefix marker


class RavPrefixR :: Symbol -> Symbol -> Symbol -> Constraint
class RavPrefixR marker head tail

instance (Symbol.Cons h t tail, RavPrefixA marker h t) => RavPrefixR marker "R" tail
else instance NoRavPrefix marker => RavPrefixR marker head tail


class RavPrefixA :: Symbol -> Symbol -> Symbol -> Constraint
class RavPrefixA marker head tail

instance (Symbol.Cons h t tail, RavPrefixV marker h t) => RavPrefixA marker "A" tail
else instance NoRavPrefix marker => RavPrefixA marker head tail


class RavPrefixV :: Symbol -> Symbol -> Symbol -> Constraint
class RavPrefixV marker head tail

instance (Symbol.Cons h t tail, RavPrefixU marker h t) => RavPrefixV marker "V" tail
else instance NoRavPrefix marker => RavPrefixV marker head tail


class RavPrefixU :: Symbol -> Symbol -> Symbol -> Constraint
class RavPrefixU marker head tail

instance RavPrefixU marker "_" tail
else instance NoRavPrefix marker => RavPrefixU marker head tail


class NoRavPrefix :: Symbol -> Constraint
class NoRavPrefix marker

instance Fail (Beside (Text "`.org` property name should start with `RAV_`: ") (Quote marker)) => NoRavPrefix marker


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
