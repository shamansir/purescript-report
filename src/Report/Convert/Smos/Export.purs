module Report.Convert.Smos.Export where

import Prelude

import Foreign (Foreign, F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Either (either)
import Data.Tuple (Tuple(..))
import Data.String (joinWith) as String
import Data.Array as Array
import Data.Newtype (unwrap)
import Data.Map (Map)
import Data.Map as Map

import Foreign.Object as Object

import Yoga.JSON (class ReadForeign, readImpl)

import Report (Report)
import Report.Core as CT
import Report.Group (Group(..))
import Report.GroupPath as GroupPath
import Report.Convert.Types
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report
import Report.Chain as Chain
import Report.Convert.Text.Decorators.Tags as CT

import Report.AtomicValue (AtomicValue(..), AtomicValueTag(..))
import Report.Decorator (Key(..), Decorator(..), encodeKey) as D
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (RawTag)
import Report.Decorators.Progress (Progress(..), PTValueTag(..), Levels(..), _vtagTo) as P
import Report.AtomicValue as AV
import Report.Decorators.Tabular.TabularValue as TV
import Report.Decorators.Tabular.Logbook (LogbookEntry, entries) as Logbook

import Data.YAML.Foreign.Encode (YValue, class ToYAML, toYAML, printYAML, object)


newtype AsYaml = AsYaml YValue

instance ToYAML AsYaml where
    toYAML (AsYaml v) = v


yv :: YValue -> YValue
yv = identity


toSmos
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toSmos inclRule =
    Report.toExport @x @subj_id @subj_tag @item_tag inclRule
        >>> unwrap
        >>> _.subjects
        >>> map (unwrap >>> subjectToDoc)
        >>> String.joinWith "\n---\n"
    where
        subjectToDoc
            :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } }
            -> String
        subjectToDoc { subject, groups } =
            let forest = buildForest [] groups
                doc = object
                    [ Tuple "version" (toYAML (2 :: Int))
                    , Tuple "value"   (toYAML $ AsYaml <$> forest)
                    ]
            in printYAML (AsYaml doc)

        buildForest :: Array String -> Array { group :: Group, items :: Array ItemRec } -> Array YValue
        buildForest parentPath pairs =
            pairs
                # Array.filter (isDirectChild parentPath)
                <#> \{ group: grp, items } ->
                    let Group grpRec = grp
                        myPath    = GroupPath.pathToArray grpRec.path
                        entryYV   = groupEntryYV grp
                        itemTrees = itemTreeYV <$> items
                        subTrees  = buildForest myPath pairs
                        forestArr = itemTrees <> subTrees
                    in treeYV entryYV forestArr

        isDirectChild :: Array String -> { group :: Group | _ } -> Boolean
        isDirectChild parentPath { group: Group grpRec } =
            let p = GroupPath.pathToArray grpRec.path
            in Array.length p == Array.length parentPath + 1
            && Array.take (Array.length parentPath) p == parentPath

        treeYV :: YValue -> Array YValue -> YValue
        treeYV entry [] = object [ Tuple "entry" entry ]
        treeYV entry forest =
            object [ Tuple "entry" entry, Tuple "forest" (toYAML $ AsYaml <$> forest) ]

        groupEntryYV :: Group -> YValue
        groupEntryYV (Group grpRec) =
            object
                [ Tuple "header"     (toYAML grpRec.title)
                , Tuple "properties" $ object [ Tuple "path" (toYAML $ GroupPath.encode grpRec.path) ]
                ]

        itemTreeYV :: ItemRec -> YValue
        itemTreeYV itemRec =
            object [ Tuple "entry" (itemEntryYV itemRec) ]

        itemEntryYV :: ItemRec -> YValue
        itemEntryYV itemRec =
            let stateHistory  = itemStateHistory itemRec
                timestamps    = itemTimestamps   itemRec
                logbookArr    = itemLogbook      itemRec
                rawTags       = unwrap itemRec.tags
                tagStrs       = (Chain.toString <<< CT.loadRawId) <$> rawTags
                mbDescription = findDecorator @D.Decorator itemRec.decorators "DESC"
                pairs = Array.catMaybes
                    [ Just $ Tuple "header" (toYAML itemRec.title)
                    , case mbDescription of
                        Just (D.SDescription desc) -> Just $ Tuple "contents" (toYAML desc)
                        _                          -> Nothing
                    , if Array.null logbookArr then Nothing
                      else Just $ Tuple "logbook" (toYAML $ AsYaml <$> logbookArr)
                    , if Array.null stateHistory then Nothing
                      else Just $ Tuple "state-history" (toYAML $ AsYaml <$> stateHistory)
                    , if Object.isEmpty timestamps then Nothing
                      else Just $ Tuple "timestamps" (toYAML $ (Map.fromFoldable :: Array _ -> Map String String) $ Object.toUnfoldable timestamps)
                    , if Array.null tagStrs then Nothing
                      else Just $ Tuple "tags" (toYAML tagStrs)
                    ]
            in object pairs

        itemLogbook :: ItemRec -> Array YValue
        itemLogbook itemRec =
            case Array.find (\{ tkey } -> tkey == "logbook") itemRec.tabulars of
                Just { value: TV.TVLogbook logbook } ->
                    logbookEntryYV <$> Logbook.entries logbook
                _ -> []

        logbookEntryYV :: Logbook.LogbookEntry -> YValue
        logbookEntryYV { start, end } =
            let startStr = maybe "1970-01-01 00:00:00.000" (smosTime <<< CT.dateFromRec) start
            in case end of
                Just ed ->
                    object
                        [ Tuple "end"   (toYAML $ smosTime $ CT.dateFromRec ed)
                        , Tuple "start" (toYAML startStr)
                        ]
                Nothing ->
                    object [ Tuple "start" (toYAML startStr) ]

        itemStateHistory :: ItemRec -> Array YValue
        itemStateHistory itemRec =
            let fromTabular = case Array.find (\{ tkey } -> tkey == "state-history") itemRec.tabulars of
                    Just { value: TV.TVLogbook logbook }
                        | not (Array.null $ Logbook.entries logbook) -> stateEntryYV <$> Logbook.entries logbook
                    _ -> []
            in if not (Array.null fromTabular) then fromTabular
               else
                   let mbTask     = findDecorator @D.Decorator itemRec.decorators $ D.encodeKey D.KTask
                       mbEarnedAt = findDecorator @D.Decorator itemRec.decorators $ D.encodeKey D.KEarnedAt
                       timeStr = case mbEarnedAt of
                           Just (D.SEarnedAt sdate) -> smosTime sdate
                           _                        -> "1970-01-01 00:00:00.000"
                       mbProgTaskP = Array.findMap
                           (\ptag ->
                               findDecorator @D.Decorator itemRec.decorators (D.encodeKey (D.KProgress (P._vtagTo ptag)))
                                   >>= case _ of
                                       D.SProgress (P.AV aval) -> atomicToSmosState aval
                                       _                       -> Nothing
                           )
                           $ map P.PTAV
                           $
                           [ AVTPTask, AVTToComplete, AVTToGetI, AVTToGetN
                           , AVTPercentI, AVTPercentN, AVTPercentSign
                           ]
                       mbTaskP = case mbTask of
                           Just (D.PTask t) -> Just t
                           _                -> mbProgTaskP
                   in case mbTaskP of
                       Just taskP ->
                           [ object
                               [ Tuple "state" (toYAML $ taskToSmosState taskP)
                               , Tuple "time"  (toYAML timeStr)
                               ]
                           ]
                       Nothing -> []

        stateEntryYV :: Logbook.LogbookEntry -> YValue
        stateEntryYV { task, start } =
            let timeStr = maybe "1970-01-01 00:00:00.000" (smosTime <<< CT.dateFromRec) start
            in object
                [ Tuple "state" (toYAML $ taskToSmosState task)
                , Tuple "time"  (toYAML timeStr)
                ]

        itemTimestamps :: ItemRec -> Object.Object String
        itemTimestamps itemRec =
            itemRec.tabulars
                # Array.mapMaybe tabularToTimestamp
                # Object.fromFoldable

        tabularToTimestamp :: TabularRec -> Maybe (Tuple String String)
        tabularToTimestamp { tkey, value } =
            let smosKey = case tkey of
                    "scheduled" -> Just "SCHEDULED"
                    "deadline"  -> Just "DEADLINE"
                    "begin"     -> Just "BEGIN"
                    "end"       -> Just "END"
                    "canceled"  -> Just "CANCELED"
                    "SCHEDULED" -> Just "SCHEDULED"
                    "DEADLINE"  -> Just "DEADLINE"
                    "BEGIN"     -> Just "BEGIN"
                    "END"       -> Just "END"
                    "CANCELED"  -> Just "CANCELED"
                    _           -> Nothing
                mbValStr = case value of
                    TV.TVAtomic (AV.OnDate sdate)         -> Just $ orgDate sdate
                    TV.TVAtomic (AV.OnDateTime sdate _)   -> Just $ orgDate sdate
                    TV.TVAtomic (AV.DateRange { from }) -> Just $ orgDate from
                    _                                     -> Nothing
            in do
                k <- smosKey
                v <- mbValStr
                pure $ Tuple k v


findDecorator :: forall @t. ReadForeign t => Array DecoratorRec -> String -> Maybe t
findDecorator decorators mkey =
    Array.findMap
        (\{ mkey: k, fvalue } ->
            if k == mkey
                then (readImpl fvalue :: F t) # runExcept # either (const Nothing) Just
                else Nothing
        )
        decorators


taskToSmosState :: TaskP -> String
taskToSmosState = case _ of
    TTodo     -> "TODO"
    TDone     -> "DONE"
    TDoing    -> "STARTED"
    TWait     -> "WAITING"
    TCanceled -> "CANCELLED"
    TNow      -> "NEXT"
    TLater    -> "LATER"


atomicToSmosState :: AtomicValue -> Maybe TaskP
atomicToSmosState = case _ of
    PTask taskP              -> Just taskP
    ToComplete { done }      -> Just $ if done then TDone else TTodo
    ToGetI { got, total }    -> Just $ if got >= total && total > 0   then TDone else if got > 0   then TDoing else TTodo
    ToGetN { got, total }    -> Just $ if got >= total && total > 0.0 then TDone else if got > 0.0 then TDoing else TTodo
    PercentI n               -> Just $ if n >= 100                    then TDone else if n > 0     then TDoing else TTodo
    PercentN n               -> Just $ if n >= 1.0                    then TDone else if n > 0.0   then TDoing else TTodo
    PercentSign { sign, pct } -> Just $ if sign >= 0 && pct >= 1.0   then TDone else if pct > 0.0 then TDoing else TTodo
    _                          -> Nothing


smosTime :: CT.SDate -> String
smosTime (CT.SDate { day, month, year }) =
       show year
    <> "-" <> CT.toLeadingZero (CT.monthToInt month)
    <> "-" <> CT.toLeadingZero day
    <> " 00:00:00.000"


orgDate :: CT.SDate -> String
orgDate (CT.SDate { day, month, year }) =
       "<" <> show year
    <> "-" <> CT.toLeadingZero (CT.monthToInt month)
    <> "-" <> CT.toLeadingZero day
    <> ">"
