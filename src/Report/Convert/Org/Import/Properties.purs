module Report.Convert.Org.Import.Properties
    ( propertyToAtomic
    , atomicFromProperty
    , propertyToDecorator
    , propertiesToDecorators
    ) where

import Prelude

import Data.Array (catMaybes) as Array
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..))
import Data.Number (fromString) as Number
import Data.String (Pattern(..), drop, indexOf, split, take, toUpper) as String
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Bifunctor (bimap)
import Foreign.Object (Object)
import Foreign.Object (toUnfoldable) as Object

import Report.Core as CT
import Report.AtomicValue (AtomicValue(..))
import Report.AtomicValue.Tag (AtomicValueTag(..))
import Report.AtomicValue.Marker.Org (atomicTagByOrgMarker)
import Data.Map (lookup) as Map
import Report.Decorator (Decorator(..)) as D
import Report.Decorators.Progress (Progress(..))
import Report.Decorators.Priority (fromString) as Priority
import Report.Decorators.Tags (RawTags(..))
import Report.Decorators.Task (mbTaskPFromString)
import Report.Convert.Rep.Import.Parser
    ( parseTime, parseDate, parseDateTime, parseRange, parseRating, parseChain, parseGroupPath
    , parseTags, splitWithSpace
    ) as P
import Utils.Data.WColor (fromHex) as WColor


-- | Reads the value properties written by `Report.Convert.Org.Export._atomicProperties`,
-- | property names are the markers from `Report.AtomicValue.Marker.Org`, case-insensitive (as everywhere in org).
propertyToAtomic :: String -> String -> Maybe AtomicValue
propertyToAtomic key value =
    Map.lookup (String.toUpper key) atomicTagByOrgMarker >>= \avTag -> atomicFromProperty avTag value


atomicFromProperty :: AtomicValueTag -> String -> Maybe AtomicValue
atomicFromProperty avTag value = case avTag of
    AVTNone          -> Just None
    AVTUnknown       -> Just Unknown
    AVTPInt          -> PInt <$> Int.fromString value
    AVTPNumber       -> PNumber <$> Number.fromString value
    AVTPText         -> Just $ PText value
    AVTPBoolean      -> PBoolean <$> bool value
    AVTPYear         -> PYear <$> Int.fromString value
    AVTPID           -> PID <$> Int.fromString value
    AVTToComplete    -> (\done -> ToComplete { done }) <$> bool value
    AVTPercentI      -> PercentI <$> Int.fromString value
    AVTPercentN      -> PercentN <$> Number.fromString value
    AVTPercentSign   -> case words of
        [ signS, pctS ] -> (\sign pct -> PercentSign { sign, pct }) <$> Int.fromString signS <*> Number.fromString pctS
        _ -> Nothing
    AVTToGetI        -> ints >>= case _ of
        [ got, total ] -> Just $ ToGetI { got, total }
        _ -> Nothing
    AVTToGetN        -> nums >>= case _ of
        [ got, total ] -> Just $ ToGetN { got, total }
        _ -> Nothing
    AVTToGetIX       -> ints >>= case _ of
        [ got, doing, total ] -> Just $ ToGetIX { got, doing, total }
        _ -> Nothing
    AVTToGetNX       -> nums >>= case _ of
        [ got, doing, total ] -> Just $ ToGetNX { got, doing, total }
        _ -> Nothing
    AVTToGetMI       -> ints >>= case _ of
        [ min, got, max ] -> Just $ ToGetMI { min, got, max }
        _ -> Nothing
    AVTToGetMN       -> nums >>= case _ of
        [ min, got, max ] -> Just $ ToGetMN { min, got, max }
        _ -> Nothing
    AVTToGetMIX      -> ints >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMIX { min, got, doing, max }
        _ -> Nothing
    AVTToGetMNX      -> nums >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMNX { min, got, doing, max }
        _ -> Nothing
    AVTOnTime        -> OnTime <$> P.parseTime value
    AVTOnDate        -> OnDate <$> P.parseDate value
    AVTOnDateTime    -> uncurry OnDateTime <$> P.parseDateTime value
    AVTTimeRange     -> TimeRange <$> fromTo <$> P.parseRange P.parseTime P.parseTime value
    AVTDateRange     -> DateRange <$> fromTo <$> P.parseRange P.parseDate P.parseDate value
    AVTDateTimeRange ->
        DateTimeRange <$> fromTo <$> bimap dateTime dateTime <$> P.parseRange P.parseDateTime P.parseDateTime value
    AVTPerI          -> amountAnd Int.fromString    <#> \{ amount, rest } -> PerI { amount, per : rest }
    AVTPerN          -> amountAnd Number.fromString <#> \{ amount, rest } -> PerN { amount, per : rest }
    AVTMeasuredI     -> amountAnd Int.fromString    <#> \{ amount, rest } -> MeasuredI { amount, measure : rest }
    AVTMeasuredN     -> amountAnd Number.fromString <#> \{ amount, rest } -> MeasuredN { amount, measure : rest }
    AVTMeasuredSign  -> case split1 value of
        Just (signS /\ rest) -> do
            sign <- Int.fromString signS
            (amountS /\ measure) <- split1 rest
            amount <- Number.fromString amountS
            pure $ MeasuredSign { sign, amount, measure }
        Nothing -> Nothing
    AVTRangeI        -> ints >>= case _ of
        [ from, to ] -> Just $ RangeI { from, to }
        _ -> Nothing
    AVTRangeN        -> nums >>= case _ of
        [ from, to ] -> Just $ RangeN { from, to }
        _ -> Nothing
    AVTPTask         -> PTask <$> mbTaskPFromString value
    AVTRelTime       -> relation P.parseTime <#> uncurry RelTime
    AVTRelInt        -> relation Int.fromString <#> uncurry RelInt
    AVTRelNum        -> relation Number.fromString <#> uncurry RelNum
    AVTPPath         -> PPath <$> P.parseChain value
    AVTTags          -> Tags <$> traverse P.parseChain (String.split (String.Pattern ",") value)
    AVTPRawTags      -> Just $ PRawTags $ RawTags $ P.parseTags value
    AVTColor         -> PColor <$> WColor.fromHex value
    AVTPRating       -> PRating <$> P.parseRating value
    AVTPPriority     -> PPriority <$> Priority.fromString value
    AVTError         -> Just $ Error value
    where
        words = P.splitWithSpace value
        ints = traverse Int.fromString words
        nums = traverse Number.fromString words
        bool = case _ of
            "true"  -> Just true
            "false" -> Just false
            _       -> Nothing
        split1 :: String -> Maybe (String /\ String)
        split1 str = String.indexOf (String.Pattern " ") str <#> \idx -> String.take idx str /\ String.drop (idx + 1) str
        amountAnd :: forall n. (String -> Maybe n) -> Maybe { amount :: n, rest :: String }
        amountAnd parseAmount = split1 value >>= \(amountS /\ rest) -> parseAmount amountS <#> \amount -> { amount, rest }
        relation :: forall a. (String -> Maybe a) -> Maybe (CT.Relation /\ a)
        relation parseFocus = split1 value >>= \(relS /\ focusS) -> (/\) <$> rel relS <*> parseFocus focusS
        rel = case _ of
            "more_than" -> Just CT.RMoreThan
            "equal"     -> Just CT.REqual
            "less_than" -> Just CT.RLessThan
            _           -> Nothing
        fromTo :: forall f t. (f /\ t) -> { from :: f, to :: t }
        fromTo (from /\ to) = { from, to }
        dateTime :: (CT.SDate /\ CT.STimeRec) -> { date :: CT.SDate, time :: CT.STimeRec }
        dateTime (date /\ time) = { date, time }


-- | Reads the decorator properties written by `Report.Convert.Org.Export`, as well as value properties.
propertyToDecorator :: String -> String -> Maybe D.Decorator
propertyToDecorator key value = case String.toUpper key of
    "RATING"        -> D.PRating <$> P.parseRating value
    "PRIORITY"      -> D.PPriority <$> Priority.fromString value
    "TASK"          -> D.PTask <$> mbTaskPFromString value
    "EARNEDAT"      -> D.SEarnedAt <$> P.parseDate value
    "DESCRIPTION"   -> Just $ D.SDescription value
    "REFERENCE"     -> D.SReference <$> P.parseGroupPath value
    "JOINREFERENCE" -> D.SJoinReference <$> P.parseGroupPath value
    _               -> D.SProgress <$> AV <$> propertyToAtomic key value


propertiesToDecorators :: Object String -> Array D.Decorator
propertiesToDecorators =
    (Object.toUnfoldable :: Object String -> Array (String /\ String))
        >>> map (uncurry propertyToDecorator)
        >>> Array.catMaybes
