module Report.Convert.Rep.Export where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (unwrap)
import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)

import Report (Report)
import Report.Core (SDateRec, STimeRec, dateToRec, toLeadingZero) as CT
import Report.Group (Group)
import Report.GroupPath (GroupPath)
import Report.GroupPath as GroupPath
import Report.Chain as MbW
import Report.Convert.Types (DecoratorRec, ItemRec, Subject)
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report
import Report.Convert.Text.Decorators.Tags (loadRawId) as CT

import Report.Decorator (Key(..), Decorator(..)) as D
import Report.Decorators.Progress (Progress(..), Relation(..))
--import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (RawTag)
import Report.Decorators.Progress (Progress(..), PTValueTag(..)) as P
--import Report.Tabular (Tabular)
import Report.Tabular (findV) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.Decorators.Tabular.TabularValue as TV
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
        mbTrackedAt tabular = Tabular.findV "trackedAt" tabular >>= case _ of
            TV.TVAtomic (TV.TVDate sdate) -> Just $ CT.dateToRec sdate
            TV.TVAtomic (TV.TVDecorator (D.SProgress (P.OnDate sdate))) -> Just $ CT.dateToRec sdate
            _ -> Nothing
        -- mbPlaytime tabular = Tabular.findV "playtime" tabular >>= case _ of
        --     TV.TVTime timeRec -> Just timeRec
        --     _ -> Nothing

        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Doc Unit
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            markTri subjKW <> D.text subjectRec.name
            <> D.break <> tabularBlock
                (Array.catMaybes
                    [ Just $ "Id" /\ tmfp P.PTText /\ (pure $ D.text $ unwrap subjectRec.id)
                    , Just $ "Platform" /\ tmfp P.PTText /\ (pure $ D.text "TODO")
                    , Just $ "Playtime" /\ tmfp P.PTText /\ (pure $ D.text "TODO")
                    , mbTrackedAt subjectRec.tabular <#> \dateRec -> "TrackedAt" /\ tmfp P.PTOnDate /\ pure (orgDate dateRec)
                    ]
                )
            <> D.break <> joinWith D.break (mapWithIndex convertGroup groups)


        decoratorsBlock :: Array (TriMarker /\ NonEmptyArray (Doc Unit)) -> Doc Unit
        decoratorsBlock [] = mempty
        decoratorsBlock fields =
            joinWith D.break $ decoratorLines <$> fields

        decoratorLines :: TriMarker /\ NonEmptyArray (Doc Unit) -> Doc Unit
        decoratorLines (tMarker /\ valueLines) =
            D.break <> markSym decKW <> markTri tMarker <> (joinWith (D.break <> markSym decKW <> markTri tMarker) $ NEA.toArray valueLines)

        tagsBlock :: Array (Doc Unit) -> Doc Unit
        tagsBlock [] = mempty
        tagsBlock tags =
            D.break <> markSym tagKW <> joinWith (D.break <> markSym tagKW) tags

        tabularBlock :: Array (String /\ TriMarker /\ NonEmptyArray (Doc Unit)) -> Doc Unit
        tabularBlock [] = mempty
        tabularBlock fields =
            joinWith D.break $ tabularLines <$> fields

        tabularLines :: String /\ TriMarker /\ NonEmptyArray (Doc Unit) -> Doc Unit
        tabularLines (name /\ tMarker /\ valueLines) =
            markSym tabKW <> D.text name
            <> D.break <> markSym tavKW <> markTri tMarker <> (joinWith (D.break <> markSym tavKW <> markTri tMarker) $ NEA.toArray valueLines)

        -- makeHeading :: Int -> Doc Unit
        -- makeHeading level =
        --     D.text (String.joinWith "" (Array.replicate level "*"))

        convertPath :: GroupPath -> Doc Unit
        convertPath path =
            joinWith D.space $ D.text <$> (unwrap <$> unwrap path)

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
                    [ "Path"  /\ tmfp P.PTText /\ pure (convertPath groupRec.path)
                    , "Index" /\ tmfp P.PTInt  /\ pure (D.text $ show index)
                    ]
                <> D.break
                <> (joinWith D.break $ D.indent <$> convertItem groupRec.path <$> items)

        convertItem :: GroupPath -> ItemRec -> Doc Unit
        convertItem grpPath itemRec =
            D.text itemRec.title
            <> case itemRec.decorators of
                  [] -> mempty
                  _ -> decoratorsBlock $ convertDecoratorToDocLine <$> itemRec.decorators
            <> case unwrap itemRec.tags of
                  [] -> mempty
                  _ -> tagsBlock $ convertTagToDocLine <$> unwrap itemRec.tags

        convertTagToDocLine :: RawTag -> Doc Unit
        convertTagToDocLine tag =
            D.text $ MbW.toString $ CT.loadRawId {- tagContent -} tag

        convertDecoratorToDocLine :: DecoratorRec -> TriMarker /\ NonEmptyArray (Doc Unit)
        convertDecoratorToDocLine modRec =
            DH.withImpl @D.Decorator
                (tmfp P.PTError /\ pure (D.text "CONVERSION ERROR"))
                (case _ of
                    D.PRating rating ->
                        tmf D.KRating /\ (pure $ D.text $ show $ Rating.toNumber rating)
                    D.PPriority priority ->
                        tmf D.KPriority /\ (pure $ D.text $ Priority.priorityChar priority)
                    D.PTask task ->
                        tmf D.KTask /\ (pure $ D.text $ Task.taskPToString task)
                    D.SProgress p ->
                        _progressProperties p
                    D.SEarnedAt ea ->
                        tmf D.KEarnedAt /\ (pure $ orgDate $ CT.dateToRec ea)
                    D.SDescription desc ->
                        tmf D.KDescription /\ (pure $ D.text desc)
                    D.SReference path ->
                        tmf D.KReference /\ (pure $ convertPath path)
                )
                modRec.fvalue


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


_progressProperties :: Progress -> TriMarker /\ NonEmptyArray (Doc Unit)
_progressProperties = case _ of
    None -> tmfp P.PTNone /\ pure (D.text ".")
    Unknown -> tmfp P.PTUnknown /\ pure (D.text ".")
    PInt i -> tmfp P.PTInt /\ pure (D.text $ show i)
    PNumber n -> tmfp P.PTNumber /\ pure (D.text $ show n)
    PText text -> tmfp P.PTText /\ pure (D.text text)
    ToComplete { done } -> tmfp P.PTToComplete /\ pure (D.text $ if done then "DONE" else "TODO")
    PercentI i -> tmfp P.PTPercentI /\ pure (D.text (show i))
    PercentN n -> tmfp P.PTPercentN /\ pure (D.text (show n))
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+" else if sign < 0 then "-" else "*"
        in tmfp P.PTPercentSign /\ pure (D.text $ sign_s <> " " <> show pct)
    ToGetI { got, total } ->
        tmfp P.PTToGetI /\ pure (D.text $ show got <> " " <> show total)
    ToGetN { got, total } ->
        tmfp P.PTToGetN /\ pure (D.text $ show got <> " " <> show total)
    OnTime timeRec ->
        tmfp P.PTOnTime /\ pure (orgTime timeRec)
    OnDate sdate ->
        tmfp P.PTOnDate /\ pure (orgDate $ CT.dateToRec sdate)
    PerI { amount, per } ->
        tmfp P.PTPerI /\ pure (D.text (show amount) <> D.text " " <> D.text per)
    PerN { amount, per } ->
        tmfp P.PTPerN /\ pure (D.text (show amount) <> D.text " " <> D.text per)
    MeasuredI { amount, measure } ->
        tmfp P.PTMeasuredI /\ pure (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredN { amount, measure } ->
        tmfp P.PTMeasuredN /\ pure (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in tmfp P.PTMeasuredSign /\ pure (D.text sign_s <> D.text " " <> D.text (show amount) <> D.text "    " <> D.text measure)
    RangeI { from, to } ->
        tmfp P.PTRangeI /\ pure (D.text $ show from <> " " <> show to)
    RangeN { from, to } ->
        tmfp P.PTRangeN /\ pure (D.text $ show from <> " " <> show to)
    Task taskP ->
        tmfp P.PTTask /\ pure (D.text $ Task.taskPToString taskP)
    LevelsI { reached, levels } ->
        tmfp P.PTLevelsI /\ pure (D.text ".") -- TODO
    LevelsN { reached, levels } ->
        tmfp P.PTLevelsN /\ pure (D.text ".") -- TODO
    LevelsS { reached, levels } ->
        tmfp P.PTLevelsS /\ pure (D.text ".") -- TODO
    LevelsE levelsE ->
        tmfp P.PTLevelsE /\ pure (D.text $ show levelsE.reached <> " " <> show levelsE.total)
    LevelsP { levels } ->
        tmfp P.PTLevelsP /\ pure (D.text ".") -- TODO
    LevelsC levelsC ->
        tmfp P.PTLevelsC /\ pure (D.text ".") -- TODO
        {-
        [ "LEVELSC_REACHED" /\ D.text (show levelsC.levelReached)
        , "LEVELSC_CURRENT" /\ D.text (show levelsC.reachedAtCurrent)
        , "LEVELSC_TOTAL" /\ D.text (show levelsC.totalLevels)
        , "LEVELSC_MAXCURRENT" /\ D.text (show levelsC.maximumAtCurrent)
        ]
        -}
    LevelsO { reached, levels } ->
        tmfp P.PTLevelsO /\ pure (D.text ".") -- TODO
    RelTime rel timeRec ->
        let relText =
                case rel of
                    RMoreThan -> ">"
                    REqual -> "="
                    RLessThan -> "<"
        in tmfp P.PTRelTime /\ pure (D.text relText <> D.space <> orgTime timeRec)
    Error err ->
        tmfp P.PTError /\ pure (D.text err) -- TODO



_progressSuffixOneLiner :: Progress -> Maybe (Doc Unit)
_progressSuffixOneLiner = case _ of
    None -> mempty
    Unknown -> mempty
    PInt i -> pure $ D.text $ show i
    PNumber n -> pure $ D.text $ show n
    PText text -> pure $ D.text text
    ToComplete { done } -> mempty -- FIXME: goes as prefix
    PercentI i -> pure $ D.text (show i) <> D.text "%"
    PercentN n -> pure $ D.text (show n) <> D.text "%"
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "" else if sign < 0 then "-" else ""
        in pure $ D.text (sign_s <> show pct <> "%")
    ToGetI { got, total } ->
        pure $ D.text (show got <> "/" <> show total)
    ToGetN { got, total } ->
        pure $ D.text (show got <> "/" <> show total)
    OnTime timeRec ->
        pure $ orgTime timeRec
    OnDate sdate ->
        pure $ orgDate (CT.dateToRec sdate)
    PerI { amount, per } ->
        pure $ D.text (show amount) <> D.text "/" <> D.text per
    PerN { amount, per } ->
        pure $ D.text (show amount) <> D.text "/" <> D.text per
    MeasuredI { amount, measure } ->
        pure $ D.text (show amount) <> D.text measure
    MeasuredN { amount, measure } ->
        pure $ D.text (show amount) <> D.text measure
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "" else if sign < 0 then "-" else ""
        in pure $ D.text sign_s <> D.text (show amount) <> D.text measure
    RangeI { from, to } ->
        pure $ D.text (show from <> "--" <> show to)
    RangeN { from, to } ->
        pure $ D.text (show from <> "--" <> show to)
    Task taskP ->
        mempty -- FIXME: goes as prefix
    LevelsI { reached, levels } ->
        mempty -- TODO
    LevelsN { reached, levels } ->
        mempty-- TODO
    LevelsS { reached, levels } ->
        mempty -- TODO
    LevelsE levelsE ->
        pure $ D.text $ show levelsE.reached <> "/" <> show levelsE.total
    LevelsP { levels } ->
        mempty -- TODO
    LevelsC levelsC ->
        mempty -- TODO
    LevelsO { reached, levels } ->
        mempty -- TODO
    RelTime rel timeRec ->
        let relText =
                case rel of
                    RMoreThan -> ">"
                    REqual -> "="
                    RLessThan -> "<"
        in pure $ D.text relText <> D.space <> orgTime timeRec
    Error err ->
        mempty