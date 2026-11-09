module Report.Convert.Rep.Export where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String as String

import Report (Report)
import Report.Class (tagContent) as CT
import Report.Core (SDateRec, STimeRec, dateToRec, dateFromRec, toLeadingZero, Relation(..), encodeRel) as CT
import Report.Group (Group)
import Report.GroupPath (GroupPath)
import Report.GroupPath as GroupPath
import Report.Chain as MbW
import Report.Convert.Types (Subject, SubjectRec, ItemRec, DecoratorRec, TabularRec)
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report
import Report.Convert.Text.Decorators.Tags (loadRawId) as CT
import Report.Convert.Keyed (encodeKey)

import Report.AtomicValue (AtomicValue(..), AtomicValueTag(..))
import Report.Decorator (Key(..), Decorator(..), keyOf) as D
import Report.Decorators.Progress (Progress(..), Levels(..))
--import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (RawTag(..), RawTags(..))
import Utils.Data.WColor (toHex) as WColor
import Report.Decorators.Progress (Progress(..), PTValueTag(..)) as P
--import Report.Tabular (Tabular)
import Report.Tabular (findV, items, Item(..)) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.AtomicValue as AV
import Report.Decorators.Tabular.TabularValue as TV
import Report.Decorators.Tabular.Logbook (entries) as Logbook
-- import Report.Convert.Text.Export as TextExport
import Report.Convert.Rep.Keys

import Report.Convert.Dhall.Export as DH

import Dodo (Doc)
import Dodo as D


markTri :: TriMarker -> Doc Unit
markTri (TM marker) = D.text marker <> D.text ". "


markSym :: SymMarker -> Doc Unit
markSym (SM marker) = D.text marker <> D.space


toRep
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toRep inclRule =
    Report.toExport @x @subj_id @subj_tag @item_tag inclRule
        >>> unwrap
        >>> _.subjects
        >>> map (unwrap >>> convertSubject)
        >>> D.lines
        >>> D.print D.plainText D.fourSpaces
        -- >>> Array.intersperse (pure "\n\n\n")
        -- >>> Array.concat
        -- >>> String.joinWith "\n"
    where
        mbTrackedAt tabular = Array.find (_.tkey >>> (_ == "trackedAt")) tabular <#> _.value >>= case _ of
            TV.TVAtomic (AV.OnDate sdate) -> Just $ CT.dateToRec sdate
            TV.TVAtomic ((OnDate sdate)) -> Just $ CT.dateToRec sdate
            _ -> Nothing
        -- mbPlaytime tabular = Tabular.findV "playtime" tabular >>= case _ of
        --     AV.OnTime timeRec -> Just timeRec
        --     _ -> Nothing

        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Doc Unit
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            markTri subjKW <> D.text subjectRec.name <> D.space <> markSym subjIDKW <> D.text (unwrap subjectRec.id)
            <> case subjectRec.tags of
                [] -> mempty
                _ -> D.break <> (tagsBlock $ convertTagToDocLine <$> subjectRec.tags)
            <> D.break <> (tabularBlock
                $ Array.catMaybes
                $ convertTabularToDocLine <$>
                    subjectRec.tabulars
                    -- (Array.catMaybes (subjTabularsToPrepend subjectRec) <> subjectRec.tabulars)
                )
            <> D.break <> joinWith D.break (mapWithIndex convertGroup groups)

        decoratorsBlock :: Array (TriMarker /\ NonEmptyArray (Doc Unit)) -> Doc Unit
        decoratorsBlock [] = mempty
        decoratorsBlock fields =
            joinWith D.break $ decoratorLines <$> fields

        decoratorLines :: TriMarker /\ NonEmptyArray (Doc Unit) -> Doc Unit
        decoratorLines (tMarker /\ valueLines) =
            markSym decKW <> markTri tMarker <> (joinWith (D.break <> markSym decKW <> markTri tMarker) $ NEA.toArray valueLines)

        tagsBlock :: Array (Doc Unit) -> Doc Unit
        tagsBlock [] = mempty
        tagsBlock tags =
            markSym tagKW <> joinWith (D.break <> markSym tagKW) tags

        subjTabularsToPrepend :: SubjectRec -> Array (Maybe TabularRec)
        subjTabularsToPrepend subjectRec =
            [ Just { tkey : "id", tlabel : "Id", value : TV.TVAtomic $ AV.PText $ unwrap subjectRec.id }
            , Just { tkey : "platform", tlabel : "Platform", value : TV.TVAtomic $ AV.PText "FOFO" }
            , Just { tkey : "playtime", tlabel : "Playtime", value : TV.TVAtomic $ AV.PText "FOFO" }
            , mbTrackedAt subjectRec.tabulars <#>
                \dateRec -> { tkey : "trackedAt", tlabel : "Tracked At", value : TV.TVAtomic $ AV.OnDate $ CT.dateFromRec dateRec }
                -- \dateRec -> "trackedAt" /\ tmfp (P.PTAV AVTOnDate) /\ pure (orgDate dateRec)
            ]

        -- makeHeading :: Int -> Doc Unit
        -- makeHeading level =
        --     D.text (String.joinWith "" (Array.replicate level "*"))

        convertGroup :: Int -> { group :: Group, items :: Array ItemRec } -> Doc Unit
        convertGroup index { group, items } =
            let
                groupRec = unwrap group
                indentGroup 0 doc = doc
                indentGroup n doc = D.indent $ indentGroup (n - 1) doc
                mbPathId = GroupPath.last groupRec.path
            in
            indentGroup (GroupPath.howDeep groupRec.path)
                $ markTri groupKW <> D.text groupRec.title
                <> case mbPathId of
                    Just pathId -> D.space <> markSym pathKW <> (D.text $ unwrap pathId)
                    Nothing -> mempty
                <> D.break <> tabularBlock
                    [ { id : "path",  label : "Path",  marker : tmfp (P.PTAV AVTPText), valueLines : pure $ convertPath groupRec.path }
                    , { id : "index", label : "Index", marker : tmfp (P.PTAV AVTPInt), valueLines : pure $ D.text $ show index        }
                    ]
                <> case items of
                    [] -> mempty
                    _ -> D.break <> (joinWith D.break $ D.indent <$> convertItem groupRec.path <$> items)

        convertItem :: GroupPath -> ItemRec -> Doc Unit
        convertItem grpPath itemRec =
            D.text itemRec.title
            <> case itemRec.decorators of
                  [] -> mempty
                  _ -> D.break <> (decoratorsBlock $ convertDecoratorToDocLine <$> itemRec.decorators)
            <> case unwrap itemRec.tags of
                  [] -> mempty
                  _ -> D.break <> (tagsBlock $ convertTagToDocLine <$> unwrap itemRec.tags)
            <> case itemRec.tabulars of
                  [] -> mempty
                  _ -> D.break <> (tabularBlock $ Array.catMaybes $ convertTabularToDocLine <$> itemRec.tabulars)

        convertTabularToDocLine :: TabularRec -> Maybe TabularRenderRec
        convertTabularToDocLine tabRec =
            _tabularValueDocLines tabRec.value <#>
                \docLines -> { id : tabRec.tkey, label : tabRec.tlabel, marker : tvMarker, valueLines : docLines }
            where
                tvMarker = case tabRec.value of
                    TV.TVAtomic atomicV -> tmft $ _tabValKeyOf atomicV
                    TV.TVValues _ -> TM "TVS"
                    TV.TVPair _ _ -> TM "TVP"
                    TV.TVValuesNest _ -> TM "TVN"
                    TV.TVTabulars _ -> TM "TVT"
                    TV.TVTabularsNest _ -> TM "TVX"
                    TV.TVLogbook _ -> TM "TVL"

        convertDecoratorToDocLine :: DecoratorRec -> TriMarker /\ NonEmptyArray (Doc Unit)
        convertDecoratorToDocLine modRec =
            DH.withImpl @D.Decorator
                (tmfp (P.PTAV AVTError) /\ pure (D.text "CONVERSION ERROR"))
                _decoratorDocLines
                modRec.fvalue


_tabularValueDocLines :: TV.TabularValue -> Maybe (NonEmptyArray (Doc Unit))
_tabularValueDocLines = case _ of
    TV.TVAtomic atomicV -> Just $ _tabularDocLines atomicV
    TV.TVValues atomicVs ->
        Array.uncons atomicVs
        <#> \{ head, tail } ->
            NEA.cons
                (prepend (SM "+++") $ D.text $ show $ Array.length atomicVs)
                (join $ _tabularDocLines <$> NEA.cons' head tail)
    TV.TVPair tabularA tabularB  ->
        (\neaA neaB ->
            NEA.cons
                (prepend (SM "+++") $ D.text $ show 2)
                $ neaA <> neaB
        )
            <$> _tabularValueDocLines tabularA
            <*> _tabularValueDocLines tabularB
    TV.TVValuesNest nestedValues ->
        case nestedValues of
            [] -> Nothing
            _ ->
                let
                    sizesString = (show $ Array.length nestedValues) <> " -> " <> String.joinWith " " (show <$> Array.length <$> nestedValues)
                    convertNestedValue (idx /\ atomicValue) = { id : show idx, label : show idx, marker : tmft $ _tabValKeyOf atomicValue, valueLines : _tabularDocLines atomicValue }
                in
                    Just $ NEA.cons'
                        (prepend (SM "+++") $ D.text sizesString)
                        (tabularLines <$> convertNestedValue <$> Array.concat (mapWithIndex (/\) <$> nestedValues))
    TV.TVTabulars tabulars ->
        case tabulars of
            [] -> Nothing
            _  ->
                let
                    convertTabItem (Tabular.Item { key, label, value }) = { id : key, label , marker : tmft $ _tabValKeyOf value, valueLines : _tabularDocLines value }
                    convertTabular tabs = tabularBlock $ convertTabItem <$> Tabular.items tabs
                in
                    Just $ NEA.cons'
                        (prepend (SM "+++") $ D.text $ show $ Array.length tabulars)
                        (convertTabular <$> tabulars)
                        -- (pure $ tabularBlock $ ?wh $ map convertTabular <$> tabulars)
    TV.TVTabularsNest { direct, parts } ->
        let
            sizesString =
                (show $ Array.length direct) <> " -> " <> (show $ Array.length parts) <> " -> " <> (String.joinWith " " $ show <$> Array.length <$> Tuple.snd <$> parts)
            convertTabItem (Tabular.Item { key, label, value }) = { id : key, label , marker : tmft $ _tabValKeyOf value, valueLines : _tabularDocLines value }
            convertTabular tabs = tabularBlock $ convertTabItem <$> Tabular.items tabs
            convertNestedValue (idx /\ atomicValue) = { id : show idx, label : show idx, marker : tmft $ _tabValKeyOf atomicValue, valueLines : _tabularDocLines atomicValue }
            convertPart (idx /\ aValue /\ partTabs) =
                tabularLines (convertNestedValue $ idx /\ aValue)
                <> D.break
                <> (prepend (SM "+++") $ D.text $ show $ Array.length partTabs)
                <> D.break
                <> joinWith D.break (convertTabular <$> partTabs)
        in
            Just $ NEA.cons'
                (prepend (SM "+++") $ D.text sizesString)
                $ (convertTabular <$> direct) <> (convertPart <$> mapWithIndex (/\) parts)
    TV.TVLogbook logbook ->
        let
            entryLine { name, task, start, end } =
                D.text ">>>" <> D.space
                    <> maybe mempty (\date -> orgDate date <> D.space) start
                    <> maybe mempty (\date -> D.text "--" <> D.space <> orgDate date <> D.space) end
                    <> D.text (Task.taskPToString task) <> D.space <> D.text name
        in
            Array.uncons (Logbook.entries logbook)
            <#> \{ head, tail } ->
                NEA.cons
                    (prepend (SM "+++") $ D.text $ show $ Array.length tail + 1)
                    (entryLine <$> NEA.cons' head tail)
        -- prepend (SM "+++") <$> (_tabularValueDocLines tabularA <> _tabularValueDocLines tabularB)


type TabularRenderRec =
    { id :: String
    , label :: String
    , marker :: TriMarker
    , valueLines :: NonEmptyArray (Doc Unit)
    }


tabularBlock :: Array TabularRenderRec -> Doc Unit
tabularBlock [] = mempty
tabularBlock fields =
    joinWith D.break $ tabularLines <$> fields


tabularLines :: TabularRenderRec -> Doc Unit
tabularLines { id, label, marker, valueLines } =
    markSym tabKW <> D.text label <> D.space <> markSym subjIDKW <> D.text id
    <> D.break <> markSym tavKW <> markTri marker <> (joinWith (D.break <> markSym tavKW <> markTri marker) $ NEA.toArray valueLines)


prepend :: SymMarker -> Doc Unit -> Doc Unit
prepend sym doc = markSym sym <> doc


joinWith :: forall a. Doc a -> Array (Doc a) -> Doc a
joinWith sep =
    fold <<< DH.mpix \d -> sep <> d


orgDate :: CT.SDateRec -> Doc Unit
orgDate dateRec
    =  D.text "<" <> D.text (show dateRec.year)
    <> D.text "-" <> D.text (CT.toLeadingZero dateRec.mon)
    <> D.text "-" <> D.text (CT.toLeadingZero dateRec.day)
    <> D.text ">"


orgTime :: CT.STimeRec -> Doc Unit
orgTime timeRec
    =  D.text (CT.toLeadingZero timeRec.hrs)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.min)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.sec)


convertPath :: GroupPath -> Doc Unit
convertPath path =
    joinWith D.space $ D.text <$> (unwrap <$> unwrap path)


convertTagToDocLine :: RawTag -> Doc Unit
convertTagToDocLine tag =
    (D.text $ MbW.toString $ CT.loadRawId {- tagContent -} tag)
    <> D.space <> D.text "//" <> D.space <> (D.text $ MbW.toString $ CT.tagContent tag)


_atomicDocLines :: AtomicValue -> TriMarker /\ NonEmptyArray (Doc Unit)
_atomicDocLines = case _ of
    None -> tmfp (P.PTAV AVTNone) /\ pure (D.text ".")
    Unknown -> tmfp (P.PTAV AVTUnknown) /\ pure (D.text ".")
    PInt i -> tmfp (P.PTAV AVTPInt) /\ pure (D.text $ show i)
    PNumber n -> tmfp (P.PTAV AVTPNumber) /\ pure (D.text $ show n)
    PText text -> tmfp (P.PTAV AVTPText) /\ pure (D.text text)
    ToComplete { done } -> tmfp (P.PTAV AVTToComplete) /\ pure (D.text $ if done then "DONE" else "TODO")
    PercentI i -> tmfp (P.PTAV AVTPercentI) /\ pure (D.text $ show i)
    PercentN n -> tmfp (P.PTAV AVTPercentN) /\ pure (D.text $ show n)
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+" else if sign < 0 then "-" else "*"
        in tmfp (P.PTAV AVTPercentSign) /\ pure (D.text $ sign_s <> " " <> show pct)
    ToGetI { got, total } ->
        tmfp (P.PTAV AVTToGetI) /\ pure (D.text $ show got <> " " <> show total)
    ToGetN { got, total } ->
        tmfp (P.PTAV AVTToGetN) /\ pure (D.text $ show got <> " " <> show total)
    OnTime timeRec ->
        tmfp (P.PTAV AVTOnTime) /\ pure (orgTime timeRec)
    OnDate sdate ->
        tmfp (P.PTAV AVTOnDate) /\ pure (orgDate $ CT.dateToRec sdate)
    PerI { amount, per } ->
        tmfp (P.PTAV AVTPerI) /\ pure (D.text (show amount) <> D.text " " <> D.text per)
    PerN { amount, per } ->
        tmfp (P.PTAV AVTPerN) /\ pure (D.text (show amount) <> D.text " " <> D.text per)
    MeasuredI { amount, measure } ->
        tmfp (P.PTAV AVTMeasuredI) /\ pure (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredN { amount, measure } ->
        tmfp (P.PTAV AVTMeasuredN) /\ pure (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+" else if sign < 0 then "-" else "*"
        in tmfp (P.PTAV AVTMeasuredSign) /\ pure (D.text sign_s <> D.text " " <> D.text (show amount) <> D.text " " <> D.text measure)
    RangeI { from, to } ->
        tmfp (P.PTAV AVTRangeI) /\ pure (D.text $ show from <> " " <> show to)
    RangeN { from, to } ->
        tmfp (P.PTAV AVTRangeN) /\ pure (D.text $ show from <> " " <> show to)
    PTask taskP ->
        tmfp (P.PTAV AVTPTask) /\ pure (D.text $ Task.taskPToString taskP)
    RelTime rel timeRec ->
        let relText =
                case rel of
                    CT.RMoreThan -> ">"
                    CT.REqual -> "="
                    CT.RLessThan -> "<"
        in tmfp (P.PTAV AVTRelTime) /\ pure (D.text relText <> D.space <> orgTime timeRec)
    RelInt rel int ->
        tmfp (P.PTAV AVTRelInt) /\ pure (D.text (CT.encodeRel rel) <> D.space <> D.text (show int))
    RelNum rel num ->
        tmfp (P.PTAV AVTRelNum) /\ pure (D.text (CT.encodeRel rel) <> D.space <> D.text (show num))
    PBoolean bool ->
        tmfp (P.PTAV AVTPBoolean) /\ pure (D.text $ if bool then "true" else "false")
    PYear year ->
        tmfp (P.PTAV AVTPYear) /\ pure (D.text $ show year)
    PID id ->
        tmfp (P.PTAV AVTPID) /\ pure (D.text $ show id)
    ToGetIX { got, doing, total } ->
        tmfp (P.PTAV AVTToGetIX) /\ pure (D.text $ show got <> " " <> show doing <> " " <> show total)
    ToGetNX { got, doing, total } ->
        tmfp (P.PTAV AVTToGetNX) /\ pure (D.text $ show got <> " " <> show doing <> " " <> show total)
    ToGetMI { min, got, max } ->
        tmfp (P.PTAV AVTToGetMI) /\ pure (D.text $ show min <> " " <> show got <> " " <> show max)
    ToGetMN { min, got, max } ->
        tmfp (P.PTAV AVTToGetMN) /\ pure (D.text $ show min <> " " <> show got <> " " <> show max)
    ToGetMIX { min, got, doing, max } ->
        tmfp (P.PTAV AVTToGetMIX) /\ pure (D.text $ show min <> " " <> show got <> " " <> show doing <> " " <> show max)
    ToGetMNX { min, got, doing, max } ->
        tmfp (P.PTAV AVTToGetMNX) /\ pure (D.text $ show min <> " " <> show got <> " " <> show doing <> " " <> show max)
    OnDateTime sdate timeRec ->
        tmfp (P.PTAV AVTOnDateTime) /\ pure (orgDateTime sdate timeRec)
    TimeRange { from, to } ->
        tmfp (P.PTAV AVTTimeRange) /\ pure (orgTime from <> D.text "--" <> orgTime to)
    DateRange { from, to } ->
        tmfp (P.PTAV AVTDateRange) /\ pure ((orgDate $ CT.dateToRec from) <> D.text "--" <> (orgDate $ CT.dateToRec to))
    DateTimeRange { from, to } ->
        tmfp (P.PTAV AVTDateTimeRange) /\ pure (orgDateTime from.date from.time <> D.text "--" <> orgDateTime to.date to.time)
    PPath path ->
        tmfp (P.PTAV AVTPPath) /\ pure (D.text $ MbW.toStringSep "::" path)
    Tags tags ->
        tmfp (P.PTAV AVTTags) /\ pure (D.text $ String.joinWith "," $ MbW.toStringSep "::" <$> tags)
    PRawTags (RawTags rawTags) ->
        tmfp (P.PTAV AVTPRawTags) /\ pure (D.text $ String.joinWith "," $ rawTagStr <$> rawTags)
    PColor color ->
        tmfp (P.PTAV AVTColor) /\ pure (D.text $ WColor.toHex color)
    PRating rating ->
        tmfp (P.PTAV AVTPRating) /\ pure (D.text $ show (Rating.toNumber rating) <> " " <> show (Rating.maxValue rating))
    PPriority priority ->
        tmfp (P.PTAV AVTPPriority) /\ pure (D.text $ Priority.priorityChar priority)
    Error err ->
        tmfp (P.PTAV AVTError) /\ pure (D.text err)
    where
        orgDateTime sdate timeRec = (orgDate $ CT.dateToRec sdate) <> D.text "@" <> orgTime timeRec
        rawTagStr (RawTag { id, content }) =
            String.joinWith "::" (NEA.toArray id) <> " // " <> String.joinWith "::" (NEA.toArray content)


_levelsDocLines :: Levels -> TriMarker /\ NonEmptyArray (Doc Unit)
_levelsDocLines = case _ of
    LevelsI { reached, levels } ->
        tmfp P.PTLevelsI /\
        let
            levelStr { date, maximum, name } =
                D.text ">>>" <> D.space <> mbDateStr date <> D.text (show maximum) <> D.space <> D.text name
        in pure $ D.text (show reached) <> D.break <> (joinWith D.break $ levelStr <$> levels)
    LevelsN { reached, levels } ->
        tmfp P.PTLevelsN /\
        let
            levelStr { date, maximum, name } =
                D.text ">>>" <> D.space <> mbDateStr date <> D.text (show maximum) <> D.space <> D.text name
        in pure $ D.text (show reached) <> D.break <> (joinWith D.break $ levelStr <$> levels)
    LevelsS { reached, levels } ->
        tmfp P.PTLevelsS /\
        let
            levelStr { gives, date } =
                D.text ">>>" <> D.space <> mbDateStr date <> D.text gives
        in pure $ D.text (show reached) <> D.break <> (joinWith D.break $ levelStr <$> levels)
    LevelsE levelsE ->
        tmfp P.PTLevelsE /\
        pure (D.text $ show levelsE.reached <> " " <> show levelsE.total)
    LevelsP { levels } ->
        tmfp P.PTLevelsP /\
        let
            levelStr { name, proc, date } =
                D.text ">>>" <> D.space <> mbDateStr date <> (D.text $ Task.taskPToString proc) <> D.space <> D.text name
        in pure $ D.text (show $ Array.length levels) <> D.break <> (joinWith D.break $ levelStr <$> levels)
    LevelsC cRec ->
        tmfp P.PTLevelsC /\ (
        pure $ D.text (show cRec.levelReached) <> D.space
            <> D.text (show cRec.totalLevels) <> D.space
            <> D.text (show cRec.reachedAtCurrent) <> D.space
            <> D.text (show cRec.maximumAtCurrent) <> case cRec.date of
                Just dateRec -> D.space <> orgDate dateRec
                Nothing -> mempty
        )
        {-
        [ "LEVELSC_REACHED" /\ D.text (show levelsC.levelReached)
        , "LEVELSC_CURRENT" /\ D.text (show levelsC.reachedAtCurrent)
        , "LEVELSC_TOTAL" /\ D.text (show levelsC.totalLevels)
        , "LEVELSC_MAXCURRENT" /\ D.text (show levelsC.maximumAtCurrent)
        ]
        -}
    LevelsO { reached, levels } ->
        tmfp P.PTLevelsO /\
        let
            levelStr { date, mbMaximum, name } =
                D.text ">>>" <> D.space <> mbDateStr date <> mbIntStr mbMaximum <> D.text name
        in pure $ D.text (show reached) <> D.break <> (joinWith D.break $ levelStr <$> levels)
    where
        mbDateStr :: Maybe CT.SDateRec -> Doc Unit
        mbDateStr = maybeSpaced orgDate
        mbIntStr :: Maybe Int -> Doc Unit
        mbIntStr = mbShowableStr
        mbShowableStr :: forall x. Show x => Maybe x -> Doc Unit
        mbShowableStr = maybeSpaced (D.text <<< show)
        maybeSpaced :: forall a. (a -> Doc Unit) -> Maybe a -> Doc Unit
        maybeSpaced toDoc = case _ of
            Just value -> toDoc value <> D.space
            Nothing -> mempty


_progressDocLines :: Progress -> TriMarker /\ NonEmptyArray (Doc Unit)
_progressDocLines = case _ of
    AV aval -> _atomicDocLines aval
    LS levels -> _levelsDocLines levels


_decoratorDocLines :: D.Decorator -> TriMarker /\ NonEmptyArray (Doc Unit)
_decoratorDocLines =
    case _ of
        D.PRating rating ->
            tmf D.KRating /\ (pure $ D.text $ show $ Rating.toNumber rating)
        D.PPriority priority ->
            tmf D.KPriority /\ (pure $ D.text $ Priority.priorityChar priority)
        D.PTask task ->
            tmf D.KTask /\ (pure $ D.text $ Task.taskPToString task)
        D.SProgress p ->
            _progressDocLines p
        D.SEarnedAt ea ->
            tmf D.KEarnedAt /\ (pure $ orgDate $ CT.dateToRec ea)
        D.SDescription desc ->
            tmf D.KDescription /\ (pure $ D.text desc)
        D.SReference path ->
            tmf D.KReference /\ (pure $ convertPath path)
        D.SJoinReference path ->
            tmf D.KJoinReference /\ (pure $ convertPath path)


_tabValKeyOf :: AtomicValue -> TV.TabValTypeKey
_tabValKeyOf = TV.typeKeyOf


_tabularDocLines :: AtomicValue -> NonEmptyArray (Doc Unit)
_tabularDocLines = case _ of
    AV.PText text  -> pure $ D.text text
    AV.PInt i        -> pure $ D.text $ show i
    AV.PNumber n     -> pure $ D.text $ show n
    AV.PID id        -> pure $ D.text $ show id
    AV.PYear year    -> pure $ D.text $ show year
    AV.OnTime timeRec -> pure $ orgTime timeRec
    AV.PBoolean bool -> pure $ D.text $ if bool then "true" else "one"
    AV.OnDate sdate   -> pure $ orgDate $ CT.dateToRec sdate
    AV.OnDateTime sdate timeRec -> pure $ (orgDate $ CT.dateToRec sdate) <> D.text "@" <> (orgTime timeRec)
    AV.TimeRange { from, to } -> pure $ (orgTime from) <> D.text "--" <> (orgTime to)
    AV.DateRange { from, to } -> pure $ (orgDate $ CT.dateToRec from) <> D.text "--" <> (orgDate $ CT.dateToRec to)
    AV.DateTimeRange { from, to } -> pure $ (orgDate $ CT.dateToRec from.date) <> D.text "@" <> (orgTime from.time) <> D.text "--" <> (orgDate $ CT.dateToRec to.date) <> D.text "@" <> (orgTime to.time)
    AV.PRawTags rawTags -> pure $ joinWith (D.text ",") $ convertTagToDocLine <$> unwrap rawTags
    aval -> Tuple.snd $ _decoratorDocLines $ TV.toDecorator aval