module Report.AtomicValue.Tag where

{- The tags of `AtomicValue`s: one for every constructor, and their textual codes used in JSON. -}

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))

import Yoga.JSON (class ReadForeign, class WriteForeign)

import Report.Core.Marker (class MarkerArgument, class GenericMarkers, toMarker, allMarkers)


data AtomicValueTag
    = AVTNone
    | AVTUnknown
    | AVTPText
    | AVTPNumber
    | AVTPInt
    | AVTPBoolean
    | AVTPYear
    | AVTPID
    | AVTToComplete
    | AVTToGetI
    | AVTToGetN
    | AVTToGetIX
    | AVTToGetNX
    | AVTToGetMI
    | AVTToGetMN
    | AVTToGetMIX
    | AVTToGetMNX
    | AVTPercentI
    | AVTPercentN
    | AVTPercentSign
    | AVTMeasuredI
    | AVTMeasuredN
    | AVTMeasuredSign
    | AVTPerI
    | AVTPerN
    | AVTOnDate
    | AVTOnTime
    | AVTOnDateTime
    | AVTTimeRange
    | AVTDateRange
    | AVTDateTimeRange
    | AVTRangeI
    | AVTRangeN
    | AVTRelTime
    | AVTRelInt
    | AVTRelNum
    | AVTPTask
    | AVTPPath
    | AVTTags
    | AVTPRawTags
    | AVTColor
    -- | AVTTaskRef
    | AVTPRating
    | AVTPPriority
    | AVTError


derive instance Generic AtomicValueTag _
derive instance Eq AtomicValueTag
derive instance Ord AtomicValueTag


-- | Markers of the constructors with an `AtomicValueTag` argument (i.e. `PTAV AtomicValueTag`)
-- | are the markers of the tag, in any format
instance (Generic AtomicValueTag rep, GenericMarkers fmt rep) => MarkerArgument fmt AtomicValueTag where
    argToMarker _ = toMarker @fmt
    argAllMarkers _ = allMarkers @fmt


newtype AVStrValueTag = AVStrValueTag String

derive newtype instance Eq  AVStrValueTag
-- derive newtype instance Ord AVStrValueTag
derive newtype instance Show AVStrValueTag
derive newtype instance ReadForeign  AVStrValueTag
derive newtype instance WriteForeign AVStrValueTag


encodeAtomicValueTag :: AtomicValueTag -> AVStrValueTag
encodeAtomicValueTag = AVStrValueTag <<< case _ of
    AVTNone ->        "E"
    AVTUnknown ->     "UNK"
    AVTPInt ->        "I"
    AVTPNumber ->     "N"
    AVTPText ->       "T"
    AVTPBoolean ->    "BOOL"
    AVTPYear ->       "YEAR"
    AVTPID ->         "UID"
    AVTToComplete ->  "D"
    AVTPercentI ->    "PCTI"
    AVTPercentN ->    "PCT"
    AVTPercentSign -> "PCTX"
    AVTToGetI ->      "PI"
    AVTToGetN ->      "PN"
    AVTOnTime ->      "TIME"
    AVTOnDate ->      "DATE"
    AVTPerI ->        "PERI"
    AVTPerN ->        "PERN"
    AVTMeasuredI ->   "MESI"
    AVTMeasuredN ->   "MESN"
    AVTMeasuredSign -> "MESX"
    AVTRangeI ->      "RNGI"
    AVTRangeN ->      "RNGN"
    AVTPTask ->       "PROC"
    AVTPPath ->       "PATH"
    AVTRelTime ->     "RELT"
    AVTRelInt ->      "RELI"
    AVTRelNum ->      "RELD"
    AVTColor ->       "COL"
    AVTError ->       "X"

    AVTToGetIX ->     "PIX"
    AVTToGetNX ->     "PNX"
    AVTToGetMIX ->    "MIX"
    AVTToGetMNX ->    "MNX"
    AVTToGetMI ->     "MI"
    AVTToGetMN ->     "MN"
    AVTOnDateTime ->  "PDT"
    AVTTimeRange ->   "PTRNG"
    AVTDateRange ->   "PDRRNG"
    AVTDateTimeRange -> "PDTRNG"

    AVTTags ->        "TAGS"
    AVTPRawTags ->    "RTAGS"
    -- AVTTaskRef ->     "TREF"
    AVTPRating ->     "PRAT"
    AVTPPriority ->   "PPRI"


decodeAtomicValueTag :: AVStrValueTag -> Maybe AtomicValueTag
decodeAtomicValueTag (AVStrValueTag avstag) = case avstag of
    "E" -> Just AVTNone
    "UNK" -> Just AVTUnknown
    "I" -> Just AVTPInt
    "N" -> Just AVTPNumber
    "F" -> Just AVTPNumber -- legacy, `Double` in Dhall
    "T" -> Just AVTPText
    "BOOL" -> Just AVTPBoolean
    "YEAR" -> Just AVTPYear
    "UID" -> Just AVTPID
    "D" -> Just AVTToComplete
    "PCTI" -> Just AVTPercentI
    "PCT" -> Just AVTPercentN
    "PCTX" -> Just AVTPercentSign
    "PI" -> Just AVTToGetI
    "PD" -> Just AVTToGetN
    "PN" -> Just AVTToGetN
    "TIME" -> Just AVTOnTime
    "DATE" -> Just AVTOnDate
    "PERI" -> Just AVTPerI
    "PERN" -> Just AVTPerN
    "PERD" -> Just AVTPerN -- legacy
    "MESI" -> Just AVTMeasuredI
    "MESD" -> Just AVTMeasuredN
    "MESN" -> Just AVTMeasuredN
    "MESX" -> Just AVTMeasuredSign
    "RNGI" -> Just AVTRangeI
    "RNGN" -> Just AVTRangeN
    "RNGD" -> Just AVTRangeN
    "PROC" -> Just AVTPTask
    "PATH" -> Just AVTPPath
    "RELT" -> Just AVTRelTime
    "RELI" -> Just AVTRelInt
    "RELN" -> Just AVTRelInt -- legacy, `Natural` in Dhall
    "RELD" -> Just AVTRelNum
    "COL"  -> Just AVTColor
    "X" -> Just AVTError

    "PIX" -> Just AVTToGetIX
    "PNX" -> Just AVTToGetNX
    "MIX" -> Just AVTToGetMIX
    "MNX" -> Just AVTToGetMNX
    "MI" -> Just AVTToGetMI
    "MN" -> Just AVTToGetMN
    "PDT" -> Just AVTOnDateTime
    "PTRNG" -> Just AVTTimeRange
    "PDRRNG" -> Just AVTDateRange
    "PDTRNG" -> Just AVTDateTimeRange

    "TAGS" -> Just AVTTags
    "RTAGS" -> Just AVTPRawTags
    -- "TREF" -> Just AVTTaskRef
    "PRAT" -> Just AVTPRating
    "PPRI" -> Just AVTPPriority

    _ -> Nothing
