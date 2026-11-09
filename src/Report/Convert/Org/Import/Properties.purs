module Report.Convert.Org.Import.Properties
    ( propertyToAtomic
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
-- | org property names are case-insensitive.
propertyToAtomic :: String -> String -> Maybe AtomicValue
propertyToAtomic key value = case String.toUpper key of
    "INTVAL"     -> PInt <$> Int.fromString value
    "NUMVAL"     -> PNumber <$> Number.fromString value
    "TEXTVAL"    -> Just $ PText value
    "BOOLVAL"    -> PBoolean <$> bool value
    "YEARVAL"    -> PYear <$> Int.fromString value
    "UIDVAL"     -> PID <$> Int.fromString value
    "TOCOMPLETE" -> (\done -> ToComplete { done }) <$> bool value
    "PERCENTI"   -> PercentI <$> Int.fromString value
    "PERCENTN"   -> PercentN <$> Number.fromString value
    "PERCENTX"   -> case words of
        [ signS, pctS ] -> (\sign pct -> PercentSign { sign, pct }) <$> Int.fromString signS <*> Number.fromString pctS
        _ -> Nothing
    "TOGETI"     -> ints >>= case _ of
        [ got, total ] -> Just $ ToGetI { got, total }
        _ -> Nothing
    "TOGETN"     -> nums >>= case _ of
        [ got, total ] -> Just $ ToGetN { got, total }
        _ -> Nothing
    "TOGETIX"    -> ints >>= case _ of
        [ got, doing, total ] -> Just $ ToGetIX { got, doing, total }
        _ -> Nothing
    "TOGETNX"    -> nums >>= case _ of
        [ got, doing, total ] -> Just $ ToGetNX { got, doing, total }
        _ -> Nothing
    "TOGETMI"    -> ints >>= case _ of
        [ min, got, max ] -> Just $ ToGetMI { min, got, max }
        _ -> Nothing
    "TOGETMN"    -> nums >>= case _ of
        [ min, got, max ] -> Just $ ToGetMN { min, got, max }
        _ -> Nothing
    "TOGETMIX"   -> ints >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMIX { min, got, doing, max }
        _ -> Nothing
    "TOGETMNX"   -> nums >>= case _ of
        [ min, got, doing, max ] -> Just $ ToGetMNX { min, got, doing, max }
        _ -> Nothing
    "ONTIME"     -> OnTime <$> P.parseTime value
    "ONDATE"     -> OnDate <$> P.parseDate value
    "ONDATETIME" -> uncurry OnDateTime <$> P.parseDateTime value
    "TIMERANGE"  -> TimeRange <$> fromTo <$> P.parseRange P.parseTime P.parseTime value
    "DATERANGE"  -> DateRange <$> fromTo <$> P.parseRange P.parseDate P.parseDate value
    "DATETIMERANGE" ->
        DateTimeRange <$> fromTo <$> bimap dateTime dateTime <$> P.parseRange P.parseDateTime P.parseDateTime value
    "PERI"       -> amountAnd Int.fromString    <#> \{ amount, rest } -> PerI { amount, per : rest }
    "PERN"       -> amountAnd Number.fromString <#> \{ amount, rest } -> PerN { amount, per : rest }
    "MESI"       -> amountAnd Int.fromString    <#> \{ amount, rest } -> MeasuredI { amount, measure : rest }
    "MESN"       -> amountAnd Number.fromString <#> \{ amount, rest } -> MeasuredN { amount, measure : rest }
    "MESX"       -> case split1 value of
        Just (signS /\ rest) -> do
            sign <- Int.fromString signS
            (amountS /\ measure) <- split1 rest
            amount <- Number.fromString amountS
            pure $ MeasuredSign { sign, amount, measure }
        Nothing -> Nothing
    "RANGEI"     -> ints >>= case _ of
        [ from, to ] -> Just $ RangeI { from, to }
        _ -> Nothing
    "RANGEN"     -> nums >>= case _ of
        [ from, to ] -> Just $ RangeN { from, to }
        _ -> Nothing
    "PROCVAL"    -> PTask <$> mbTaskPFromString value
    "RELTIME"    -> relation P.parseTime <#> uncurry RelTime
    "RELINT"     -> relation Int.fromString <#> uncurry RelInt
    "RELNUM"     -> relation Number.fromString <#> uncurry RelNum
    "PATHVAL"    -> PPath <$> P.parseChain value
    "TAGSVAL"    -> Tags <$> traverse P.parseChain (String.split (String.Pattern ",") value)
    "RAWTAGSVAL" -> Just $ PRawTags $ RawTags $ P.parseTags value
    "COLORVAL"   -> PColor <$> WColor.fromHex value
    "RATINGVAL"  -> PRating <$> P.parseRating value
    "PRIORITYVAL" -> PPriority <$> Priority.fromString value
    "ERRORVAL"   -> Just $ Error value
    _            -> Nothing
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
