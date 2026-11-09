module Report.Convert.Text.Decorators.Progress where

import Prelude

import Data.Array.NonEmpty (fromArray, toArray) as NEA
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Number (fromString) as Number
import Data.String (split, joinWith, trim, Pattern(..)) as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\), type (/\))
import Report.Core (SDate(..), STimeRec, monthFromInt, monthToInt, toLeadingZero, Relation(..)) as CT
import Report.Core (Relation(..), encodeRel, decodeRel)
import Report.Core.Logic (EncodedValue(..)) as CT
import Report.AtomicValue (AtomicValue(..), AtomicValueTag(..), AVStrValueTag(..), decodeAtomicValueTag)
import Report.Chain (Chain)
import Report.Chain (fromArray, toStringSep) as Chain
import Report.Decorators.Priority (fromString, priorityChar) as Priority
import Report.Decorators.Rating (Rating(..))
import Report.Decorators.Tags (RawTag(..), RawTags(..))
import Utils.Data.WColor (fromHex, toHex) as WColor
import Report.Decorators.Progress (PValueTag(..), Progress(..), unwrapValueTag, valueTagOf)
import Report.Decorators.Task (taskPFromString, taskPToString)


encodeValueTag :: PValueTag -> String
encodeValueTag = unwrapValueTag


decodeValueTag :: String -> Maybe PValueTag
decodeValueTag str =
    if isJust (decodeAtomicValueTag $ AVStrValueTag str) || isLevelsTag
        then Just $ PValueTag str
        else Nothing
    where
        isLevelsTag = case str of
            "LVLI" -> true
            "LVLD" -> true
            "LVLO" -> true
            "LVLS" -> true
            "LVLP" -> true
            "LVLC" -> true
            "LVLE" -> true
            _ -> false


encodeDate :: CT.SDate -> String
encodeDate (CT.SDate { day, month, year }) =
    CT.toLeadingZero day <> "-" <> CT.toLeadingZero (CT.monthToInt month) <> "-" <> show year


decodeDate :: String -> Maybe CT.SDate
decodeDate evStr = case qsplit "-" evStr of
    [ dayStr, monStr, yearStr ] ->
        (\day mon year -> CT.SDate { day, month : CT.monthFromInt mon, year })
        <$> Int.fromString dayStr
        <*> Int.fromString monStr
        <*> Int.fromString yearStr
    _ -> Nothing


encodeTime :: CT.STimeRec -> String
encodeTime { hrs, min, sec } = show hrs <> ":" <> CT.toLeadingZero min <> ":" <> CT.toLeadingZero sec


decodeTime :: String -> Maybe CT.STimeRec
decodeTime evStr = case qsplit ":" evStr of
    [ hrsStr, minStr, secStr ] ->
        (\hrs min sec -> { hrs, min, sec })
        <$> Int.fromString hrsStr
        <*> Int.fromString minStr
        <*> Int.fromString secStr
    [ hrsStr, minStr ] ->
        (\hrs min -> { hrs, min, sec : 0 })
        <$> Int.fromString hrsStr
        <*> Int.fromString minStr
    _ -> Nothing


_encodeAtomic :: AtomicValue -> CT.EncodedValue
_encodeAtomic = CT.EncodedValue <<< case _ of
    None -> ""
    Unknown -> ""
    PInt i -> show i
    PNumber n -> show n
    PText text -> text
    PBoolean b -> if b then "true" else "false"
    PYear y -> show y
    PID id -> show id
    ToComplete { done } -> if done then "DONE" else "TODO"
    PercentI i -> show i
    PercentN n -> show n
    PercentSign { sign, pct } -> encodeSign sign <> ";" <> show pct
    ToGetI { got, total } -> show got <> "/" <> show total
    ToGetN { got, total } -> show got <> "/" <> show total
    ToGetIX { got, doing, total } -> show got <> "/" <> show doing <> "/" <> show total
    ToGetNX { got, doing, total } -> show got <> "/" <> show doing <> "/" <> show total
    ToGetMI { min, got, max } -> show min <> ";" <> show got <> ";" <> show max
    ToGetMN { min, got, max } -> show min <> ";" <> show got <> ";" <> show max
    ToGetMIX { min, got, doing, max } -> show min <> ";" <> show got <> ";" <> show doing <> ";" <> show max
    ToGetMNX { min, got, doing, max } -> show min <> ";" <> show got <> ";" <> show doing <> ";" <> show max
    OnTime { hrs, min, sec } -> encodeTime { hrs, min, sec }
    OnDate stDate -> encodeDate stDate
    OnDateTime stDate time -> encodeDate stDate <> " " <> encodeTime time
    TimeRange { from, to } -> encodeTime from <> ";" <> encodeTime to
    DateRange { from, to } -> encodeDate from <> ";" <> encodeDate to
    DateTimeRange { from, to } ->
        encodeDate from.date <> " " <> encodeTime from.time <> ";" <> encodeDate to.date <> " " <> encodeTime to.time
    PerI { amount, per } -> show amount <> ";" <> per
    PerN { amount, per } -> show amount <> ";" <> per
    MeasuredI { amount, measure } -> show amount <> ";" <> measure
    MeasuredN { amount, measure } -> show amount <> ";" <> measure
    MeasuredSign { sign, amount, measure } -> encodeSign sign <> ";" <> show amount <> ";" <> measure
    RangeI { from, to } -> show from <> rangeSep (from < 0 || to < 0) <> show to
    RangeN { from, to } -> show from <> rangeSep (from < 0.0 || to < 0.0) <> show to
    PTask taskP -> taskPToString taskP
    RelTime rel timeRec -> encodeRel rel <> ";" <> encodeTime timeRec
    RelInt rel int -> encodeRel rel <> ";" <> show int
    RelNum rel num -> encodeRel rel <> ";" <> show num
    PPath path -> Chain.toStringSep "::" path
    Tags tags -> String.joinWith "," $ Chain.toStringSep "::" <$> tags
    PRawTags (RawTags rawTags) -> String.joinWith "," $ encodeRawTag <$> rawTags
    PColor color -> WColor.toHex color
    PRating (Rating { value, max }) -> show value <> "/" <> show max
    PPriority priority -> Priority.priorityChar priority
    Error err -> err
    where
        encodeSign sign = if sign > 0 then "+" else if sign < 0 then "-" else "0"
        rangeSep hasNegative = if hasNegative then "--" else "-"
        encodeRawTag (RawTag { id, content }) =
            String.joinWith "::" (NEA.toArray id) <> " // " <> String.joinWith "::" (NEA.toArray content)


encodeProgress :: Progress -> PValueTag /\ CT.EncodedValue
encodeProgress p = valueTagOf p /\ (case p of
    AV aval -> _encodeAtomic aval
    LS levels -> CT.EncodedValue "" -- FIXME: TODO
    )


decodeProgress :: PValueTag -> CT.EncodedValue -> Maybe Progress
decodeProgress pvt encv = _decodeAtomic pvt encv <#> AV


_decodeAtomic :: PValueTag -> CT.EncodedValue -> Maybe AtomicValue
_decodeAtomic pvt encv =
    decodeAtomicValueTag (AVStrValueTag $ unwrapValueTag pvt)
        >>= \avTag -> _decodeAtomicByTag avTag encv


_decodeAtomicByTag :: AtomicValueTag -> CT.EncodedValue -> Maybe AtomicValue
_decodeAtomicByTag avTag (CT.EncodedValue evStr) = case avTag of
    AVTNone -> Just None
    AVTUnknown -> Just Unknown
    AVTPInt -> PInt <$> Int.fromString evStr
    AVTPNumber -> PNumber <$> Number.fromString evStr
    AVTPText -> Just $ PText evStr
    AVTPBoolean -> case evStr of
        "true" -> Just $ PBoolean true
        "false" -> Just $ PBoolean false
        _ -> Nothing
    AVTPYear -> PYear <$> Int.fromString evStr
    AVTPID -> PID <$> Int.fromString evStr
    AVTToComplete -> case evStr of
        "DONE" -> Just $ ToComplete { done : true }
        "TODO" -> Just $ ToComplete { done : false }
        _ -> Nothing
    AVTPercentI -> PercentI <$> Int.fromString evStr
    AVTPercentN -> PercentN <$> Number.fromString evStr
    AVTPercentSign -> case qsplit ";" evStr of
        [ signStr, pctStr ] ->
            (\sign pct -> PercentSign { sign, pct })
            <$> signFrom signStr
            <*> Number.fromString pctStr
        [ pctStr ] -> Number.fromString pctStr <#> (\pct -> PercentSign { sign : 1, pct })
        _ -> Nothing
    AVTToGetI -> traverse Int.fromString (qsplit "/" evStr) >>= case _ of
        [ got, total ] -> Just $ ToGetI { got, total }
        _ -> Nothing
    AVTToGetN -> traverse Number.fromString (qsplit "/" evStr) >>= case _ of
        [ got, total ] -> Just $ ToGetN { got, total }
        _ -> Nothing
    AVTToGetIX -> traverse Int.fromString (qsplit "/" evStr) >>= case _ of
        [ got, doing, total ] -> Just $ ToGetIX { got, doing, total }
        _ -> Nothing
    AVTToGetNX -> traverse Number.fromString (qsplit "/" evStr) >>= case _ of
        [ got, doing, total ] -> Just $ ToGetNX { got, doing, total }
        _ -> Nothing
    AVTToGetMI -> traverse Int.fromString (qsplit ";" evStr) >>= case _ of
        [ min, got, max ] -> Just $ ToGetMI { min, got, max }
        _ -> Nothing
    AVTToGetMN -> traverse Number.fromString (qsplit ";" evStr) >>= case _ of
        [ min, got, max ] -> Just $ ToGetMN { min, got, max }
        _ -> Nothing
    AVTToGetMIX -> traverse Int.fromString (qsplit ";" evStr) >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMIX { min, got, doing, max }
        _ -> Nothing
    AVTToGetMNX -> traverse Number.fromString (qsplit ";" evStr) >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMNX { min, got, doing, max }
        _ -> Nothing
    AVTOnTime -> decodeTime evStr <#> OnTime
    AVTOnDate -> decodeDate evStr <#> OnDate
    AVTOnDateTime -> decodeDateTime evStr <#> \{ date, time } -> OnDateTime date time
    AVTTimeRange -> case qsplit ";" evStr of
        [ fromStr, toStr ] -> (\from to -> TimeRange { from, to }) <$> decodeTime fromStr <*> decodeTime toStr
        _ -> Nothing
    AVTDateRange -> case qsplit ";" evStr of
        [ fromStr, toStr ] -> (\from to -> DateRange { from, to }) <$> decodeDate fromStr <*> decodeDate toStr
        _ -> Nothing
    AVTDateTimeRange -> case qsplit ";" evStr of
        [ fromStr, toStr ] -> (\from to -> DateTimeRange { from, to }) <$> decodeDateTime fromStr <*> decodeDateTime toStr
        _ -> Nothing
    AVTPerI -> case qsplit ";" evStr of
        [ amountStr, per ] -> (\amount -> PerI { amount, per }) <$> Int.fromString amountStr
        _ -> Nothing
    AVTPerN -> case qsplit ";" evStr of
        [ amountStr, per ] -> (\amount -> PerN { amount, per }) <$> Number.fromString amountStr
        _ -> Nothing
    AVTMeasuredI -> case qsplit ";" evStr of
        [ amountStr, measure ] -> (\amount -> MeasuredI { amount, measure }) <$> Int.fromString amountStr
        _ -> Nothing
    AVTMeasuredN -> case qsplit ";" evStr of
        [ amountStr, measure ] -> (\amount -> MeasuredN { amount, measure }) <$> Number.fromString amountStr
        _ -> Nothing
    AVTMeasuredSign -> case qsplit ";" evStr of
        [ signStr, amountStr, measure ] ->
            (\sign amount -> MeasuredSign { sign, amount, measure })
            <$> signFrom signStr
            <*> Number.fromString amountStr
        [ amountStr, measure ] ->
            (\amount -> MeasuredSign { sign : 1, amount, measure })
            <$> Number.fromString amountStr
        _ -> Nothing
    AVTRangeI -> case rangeSplit evStr of
        [ fromStr, toStr ] -> (\from to -> RangeI { from, to }) <$> Int.fromString fromStr <*> Int.fromString toStr
        _ -> Nothing
    AVTRangeN -> case rangeSplit evStr of
        [ fromStr, toStr ] -> (\from to -> RangeN { from, to }) <$> Number.fromString fromStr <*> Number.fromString toStr
        _ -> Nothing
    AVTPTask -> Just $ PTask $ taskPFromString evStr
    AVTRelTime -> case qsplit ";" evStr of
        [ relStr, timeStr ] -> RelTime (decodeRel relStr) <$> decodeTime timeStr
        _ -> Nothing
    AVTRelInt -> case qsplit ";" evStr of
        [ relStr, intStr ] -> RelInt (decodeRel relStr) <$> Int.fromString intStr
        _ -> Nothing
    AVTRelNum -> case qsplit ";" evStr of
        [ relStr, numStr ] -> RelNum (decodeRel relStr) <$> Number.fromString numStr
        _ -> Nothing
    AVTPPath -> PPath <$> decodeChain evStr
    AVTTags -> Tags <$> traverse decodeChain (qsplit "," evStr)
    AVTPRawTags -> PRawTags <$> RawTags <$> traverse decodeRawTag (qsplit "," evStr)
    AVTColor -> PColor <$> WColor.fromHex evStr
    AVTPRating -> case qsplit "/" evStr of
        [ valueStr, maxStr ] ->
            (\value max -> PRating $ Rating { value, max })
            <$> Number.fromString valueStr
            <*> Int.fromString maxStr
        _ -> Nothing
    AVTPPriority -> PPriority <$> Priority.fromString evStr
    AVTError -> Just $ Error evStr
    where
        signFrom "+" = Just $  1
        signFrom "-" = Just $ -1
        signFrom "0" = Just $  0
        signFrom _   = Nothing
        rangeSplit str = case qsplit "--" str of
            [ fromStr, toStr ] -> [ fromStr, toStr ]
            _ -> qsplit "-" str
        decodeDateTime :: String -> Maybe { date :: CT.SDate, time :: CT.STimeRec }
        decodeDateTime str = case qsplit " " str of
            [ dateStr, timeStr ] -> (\date time -> { date, time }) <$> decodeDate dateStr <*> decodeTime timeStr
            _ -> Nothing
        decodeChain :: String -> Maybe (Chain String)
        decodeChain = qsplit "::" >>> Chain.fromArray
        decodeRawTag :: String -> Maybe RawTag
        decodeRawTag str = case qsplit " // " str of
            [ idStr, contentStr ] ->
                (\id content -> RawTag { id, content })
                <$> NEA.fromArray (qsplit "::" $ String.trim idStr)
                <*> NEA.fromArray (qsplit "::" $ String.trim contentStr)
            _ -> Nothing


qsplit :: String -> String -> Array String
qsplit sep = String.split (String.Pattern sep)
