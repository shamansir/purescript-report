module Report.Encoding.Modifiers.Progress where

import Prelude

import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.String (split, Pattern(..)) as String
import Data.Tuple.Nested ((/\), type (/\))
import Report.Core as CT
import Report.Modifiers.Progress (PValueTag(..), Progress(..), unwrapValueTag, valueTagOf)
import Report.Modifiers.Task (taskPFromString, taskPToString)


encodeValueTag :: PValueTag -> String
encodeValueTag = unwrapValueTag


decodeValueTag :: String -> Maybe PValueTag
decodeValueTag = case _ of
    "E" -> Just $ PValueTag "E"
    "UNK" -> Just $ PValueTag "UNK"
    "I" -> Just $ PValueTag "I"
    "N" -> Just $ PValueTag "N"
    "T" -> Just $ PValueTag "T"
    "D" -> Just $ PValueTag "D"
    "PCTI" -> Just $ PValueTag "PCTI"
    "PCT" -> Just $ PValueTag "PCT"
    "PCTX" -> Just $ PValueTag "PCTX"
    "PI" -> Just $ PValueTag "PI"
    "PD" -> Just $ PValueTag "PD"
    "TIME" -> Just $ PValueTag "TIME"
    "DATE" -> Just $ PValueTag "DATE"
    "PERI" -> Just $ PValueTag "PERI"
    "PERD" -> Just $ PValueTag "PERD"
    "MESI" -> Just $ PValueTag "MESI"
    "MESD" -> Just $ PValueTag "MESD"
    "MESX" -> Just $ PValueTag "MESX"
    "RNGI" -> Just $ PValueTag "RNGI"
    "RNGD" -> Just $ PValueTag "RNGD"
    "PROC" -> Just $ PValueTag "PROC"
    "LVLI" -> Just $ PValueTag "LVLI"
    "LVLD" -> Just $ PValueTag "LVLD"
    "LVLO" -> Just $ PValueTag "LVLO"
    "LVLS" -> Just $ PValueTag "LVLS"
    "LVLP" -> Just $ PValueTag "LVLP"
    "LVLC" -> Just $ PValueTag "LVLC"
    "LVLE" -> Just $ PValueTag "LVLE"
    "X" -> Just $ PValueTag "X"
    _ -> Nothing


encodeDate :: CT.SDate -> String
encodeDate (CT.SDate { day, month, year }) =
    show day <> "-" <> show (CT.monthToInt month) <> "-" <> show year


decodeDate :: String -> Maybe CT.SDate
decodeDate evStr = case qsplit "-" evStr of
    [ dayStr, monStr, yearStr ] ->
        (\day mon year -> CT.SDate { day, month : CT.monthFromInt mon, year })
        <$> Int.fromString dayStr
        <*> Int.fromString monStr
        <*> Int.fromString yearStr
    _ -> Nothing


encodeTime :: CT.STimeRec -> String
encodeTime { hrs, min, sec } = show hrs <> ":" <> show min <> ":" <> show sec


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


encodeProgress :: Progress -> PValueTag /\ CT.EncodedValue
encodeProgress p = valueTagOf p /\ (CT.EncodedValue $ case p of
    None -> ""
    Unknown -> ""
    PInt i -> show i
    PNumber n -> show n
    PText text -> text
    ToComplete { done } -> if done then "DONE" else "TODO"
    PercentI i -> show i
    PercentN n -> show n
    PercentSign { sign, pct } -> show sign <> ";" <> show pct
    ToGetI { got, total } -> show got <> "/" <> show total
    ToGetN { got, total } -> show got <> "/" <> show total
    OnTime { hrs, min, sec } -> encodeTime { hrs, min, sec }
    OnDate stDate -> encodeDate stDate
    PerI { amount, per } -> show amount <> ";" <> per
    PerN { amount, per } -> show amount <> ";" <> per
    MeasuredI { amount, measure } -> show amount <> ";" <> measure
    MeasuredN { amount, measure } -> show amount <> ";" <> measure
    MeasuredSign { sign, amount, measure } -> show sign <> ";" <> show amount <> ";" <> measure
    RangeI { from, to } -> show from <> "-" <> show to
    RangeN { from, to } -> show from <> "-" <> show to
    Task taskP -> taskPToString taskP
    LevelsI _ -> "" -- TODO
    LevelsN _ -> "" -- TODO
    LevelsO _ -> "" -- TODO
    LevelsS _ -> "" -- TODO
    LevelsP _ -> "" -- TODO
    LevelsC _ -> "" -- TODO
    LevelsE _ -> "" -- TODO
    Error err -> err
    )


decodeProgress :: PValueTag -> CT.EncodedValue -> Maybe Progress
decodeProgress pvt (CT.EncodedValue evStr) = case unwrapValueTag pvt of
    "E" -> Just None
    "UNK" -> Just Unknown
    "I" -> PInt <$> Int.fromString evStr
    "N" -> PNumber <$> Number.fromString evStr
    "T" -> Just $ PText evStr
    "D" -> case evStr of
        "DONE" -> Just $ ToComplete { done : true }
        "TODO" -> Just $ ToComplete { done : false }
        _ -> Nothing
    "PCTI" -> PercentI <$> Int.fromString evStr
    "PCT"  -> PercentN <$> Number.fromString evStr
    "PCTX" -> case qsplit ";" evStr of
        [ signStr, pctStr ] ->
            (\sign pct -> PercentSign { sign, pct })
            <$> signFrom signStr
            <*> Number.fromString pctStr
        [ pctStr ] -> Number.fromString pctStr <#> (\pct -> PercentSign { sign : 1, pct })
        _ -> Nothing
    "PI" -> case qsplit "/" evStr of
        [ gotStr, totalStr ] ->
            (\got total -> ToGetI { got, total })
            <$> Int.fromString gotStr
            <*> Int.fromString totalStr
        _ -> Nothing
    "PD" -> case qsplit "/" evStr of
        [ gotStr, totalStr ] ->
            (\got total -> ToGetN { got, total })
            <$> Number.fromString gotStr
            <*> Number.fromString totalStr
        _ -> Nothing
    "TIME" -> decodeTime evStr <#> OnTime
    "DATE" -> decodeDate evStr <#> OnDate
    "PERI" -> case qsplit ";" evStr of
        [ amountStr, per ] ->
            (\amount per -> PerI { amount, per })
            <$> Int.fromString amountStr
            <*> Just per
        _ -> Nothing
    "PERD" -> case qsplit ";" evStr of
        [ amountStr, per ] ->
            (\amount per -> PerN { amount, per })
            <$> Number.fromString amountStr
            <*> Just per
        _ -> Nothing
    "MESI" -> case qsplit ";" evStr of
        [ amountStr, measure ] ->
            (\amount -> MeasuredI { amount, measure })
            <$> Int.fromString amountStr
        _ -> Nothing
    "MESD" -> case qsplit ";" evStr of
        [ amountStr, measure ] ->
            (\amount -> MeasuredN { amount, measure })
            <$> Number.fromString amountStr
        _ -> Nothing
    "MESX" -> case qsplit ";" evStr of
        [ signStr, amountStr, measure ] ->
            (\sign amount -> MeasuredSign { sign, amount, measure })
            <$> signFrom signStr
            <*> Number.fromString amountStr
        [ amountStr, measure ] ->
            (\amount -> MeasuredSign { sign : 1, amount, measure })
            <$> Number.fromString amountStr
        _ -> Nothing
    "RNGI" -> case qsplit "-" evStr of
        [ fromStr, toStr ] ->
            (\from to -> RangeI { from, to })
            <$> Int.fromString fromStr
            <*> Int.fromString toStr
        _ -> Nothing
    "RNGD" -> case qsplit "-" evStr of
        [ fromStr, toStr ] ->
            (\from to -> RangeN { from, to })
            <$> Number.fromString fromStr
            <*> Number.fromString toStr
        _ -> Nothing
    "PROC" -> Just $ Task $ taskPFromString evStr
    "LVLI" -> Nothing -- TODO
    "LVLD" -> Nothing -- TODO
    "LVLO" -> Nothing -- TODO
    "LVLS" -> Nothing -- TODO
    "LVLP" -> Nothing -- TODO
    "LVLC" -> Nothing -- TODO
    "LVLE" -> Nothing -- TODO
    "X" -> Just $ Error evStr
    _ -> Nothing
    where
        signFrom "+" = Just $  1
        signFrom "-" = Just $ -1
        signFrom _   = Nothing


qsplit sep = String.split (String.Pattern sep)