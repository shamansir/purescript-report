module Report.Convert.Org.Export where

import Prelude

import Foreign (Foreign, F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (uncurry) as Tuple
import Data.String (joinWith, replaceAll, Pattern(..), Replacement(..)) as String
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Array ((:))
import Data.Array as Array
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)

import Yoga.JSON (class WriteForeign, writePrettyJSON, class ReadForeign, readImpl)

import Report (Report)
import Report.Core as CT
import Report.Group (Group)
import Report.GroupPath (GroupPath)
import Report.GroupPath as GroupPath
import Report.Chain as MbW
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag, tagContent)
import Report.Convert.Keyed (class EncodableKey, decodeKey)
import Report.Convert.Types
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report
import Report.Convert.Text.Decorators.Tags as CT

import Report.Decorator (Key(..), Decorator(..)) as D
import Report.Decorators.Progress (Progress(..), PValueTag(..), Relation(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (Tags, RawTag)
import Report.Decorators.Progress (Progress(..)) as P
import Report.Tabular (Tabular)
import Report.Tabular (findV) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.Decorators.Tabular.TabularValue as TV
-- import Report.Convert.Text.Export as TextExport

import Report.Convert.Dhall.Export as DH

import Dodo
import Dodo as D


toOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toOrg inclRule =
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
            TV.TVAtomic (TV.TVDate sdate) -> Just $ CT.dateToRec sdate
            TV.TVAtomic (TV.TVDecorator (D.SProgress (P.OnDate sdate))) -> Just $ CT.dateToRec sdate
            _ -> Nothing
        -- mbPlaytime tabular = Tabular.findV "playtime" tabular >>= case _ of
        --     TV.TVTime timeRec -> Just timeRec
        --     _ -> Nothing

        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Doc Unit
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            D.text "* " <> D.text subjectRec.name
            <> D.break <> propertiesBlock
                (Array.catMaybes
                    [ Just $ "Id" /\ D.text (unwrap subjectRec.id)
                    , Just $ "Platform" /\ D.text "TODO"
                    , Just $ "Playtime" /\ D.text "TODO"
                    , mbTrackedAt subjectRec.tabulars <#> \dateRec -> "TrackedAt" /\ orgDate dateRec
                    ]
                )
            <> D.break <> joinWith D.break (mapWithIndex convertGroup groups)

        propertiesBlock :: Array (String /\ Doc Unit) -> Doc Unit
        propertiesBlock [] = mempty
        propertiesBlock fields =
            D.text ":PROPERTIES:" <> D.break
            <> (joinWith D.break $ propertyLine <$> fields)
            <> D.break <> D.text ":END:"

        propertyLine :: (String /\ Doc Unit) -> Doc Unit
        propertyLine (name /\ value) = D.text ":" <> D.text name <> D.text ": " <> value

        makeHeading :: Int -> Doc Unit
        makeHeading level =
            D.text (String.joinWith "" (Array.replicate level "*"))

        convertPath :: GroupPath -> Doc Unit
        convertPath path =
            joinWith D.space $ D.text <$> (unwrap <$> unwrap path)

        convertGroup :: Int -> { group :: Group, items :: Array ItemRec } -> Doc Unit
        convertGroup index { group, items } =
            let
                groupRec = unwrap group
                groupHeadingPrefix = makeHeading $ GroupPath.howDeep groupRec.path + 1
            in
            groupHeadingPrefix <+> D.text groupRec.title
            <> D.break <> propertiesBlock
                [ "Path" /\ convertPath groupRec.path
                , "Index" /\ D.text (show index)
                ]
            <> D.break
            <> (joinWith D.break $ convertItem groupRec.path <$> items)

        convertItem :: GroupPath -> ItemRec -> Doc Unit
        convertItem grpPath itemRec =
            let
                itemHeadingPrefix = makeHeading $ GroupPath.howDeep grpPath + 2
                decoratorsPrefixes   = Array.catMaybes $ convertDecoratorToPrefix   <$> itemRec.decorators
                decoratorsSuffixes   = Array.catMaybes $ convertDecoratorToSuffix   <$> itemRec.decorators
                decoratorsProperties = Array.concat    $ convertDecoratorToProperty <$> itemRec.decorators
                mbTagsSuffix         = convertTagsToSuffix $ unwrap itemRec.tags
                mbTagsProperty       = convertTagsToProperty $ unwrap itemRec.tags
                titleProperty = ("Title" /\ D.text itemRec.title)
                decoratorsPrefixesDoc =
                    case decoratorsPrefixes of
                        [] -> mempty
                        prefixes -> joinWith D.space prefixes <> D.space
                decoratorsSuffixesDoc =
                    case decoratorsSuffixes of
                        [] -> mempty
                        suffixes -> D.space <> joinWith D.space suffixes
                propertiesBlockDoc =
                    case decoratorsProperties of
                        [] -> case mbTagsProperty of
                            Just tagsProp -> D.break <> propertiesBlock ( titleProperty : pure tagsProp )
                            Nothing -> mempty
                        props -> case mbTagsProperty of
                            Just tagsProp -> D.break <> propertiesBlock ( titleProperty : tagsProp : props )
                            Nothing -> D.break <> propertiesBlock ( titleProperty : props )
            in
            itemHeadingPrefix <+> decoratorsPrefixesDoc <> D.text itemRec.title <> decoratorsSuffixesDoc
            <> case mbTagsSuffix of
                Just tagsSuffix -> D.space <> tagsSuffix
                Nothing -> mempty
            <> propertiesBlockDoc

        convertTagsToProperty :: Array RawTag -> Maybe (String /\ Doc Unit)
        convertTagsToProperty = case _ of
            [] -> Nothing
            tags ->
                Just $ "Tags" /\ DH.joinWith D.space (D.text <$> MbW.toString <$> CT.loadRawId {- tagContent -} <$> tags)

        convertDecoratorToProperty :: DecoratorRec -> Array (String /\ Doc Unit)
        convertDecoratorToProperty modRec =
            DH.withImpl @D.Decorator
                []
                (case _ of
                    D.PRating rating ->
                        pure $ "Rating" /\ (D.text $ show $ Rating.toNumber rating)
                    D.PPriority priority ->
                        pure $ "Priority" /\ (D.text $ Priority.priorityChar priority)
                    D.PTask task ->
                        pure $ "Task" /\ (D.text $ Task.taskPToString task)
                    D.SProgress p ->
                        _progressProperties p
                    D.SEarnedAt ea ->
                        pure $ "EarnedAt" /\ orgDate (CT.dateToRec ea)
                    D.SDescription desc ->
                        pure $ "Description" /\ D.text desc
                    D.SReference path ->
                        pure $ "Reference" /\ convertPath path
                )
                modRec.fvalue

        convertDecoratorToPrefix :: DecoratorRec -> Maybe (Doc Unit)
        convertDecoratorToPrefix modRec =
            DH.withImpl @D.Decorator -- TODO: export tags to strings beforehand
                mempty
                (case _ of
                    D.PRating rating ->
                        Just $ D.text $ show $ Rating.toStars rating
                    D.PPriority priority ->
                        Just $ D.text "[#" <> D.text (Priority.priorityChar priority) <> D.text "]"
                    D.PTask task ->
                        Just $ D.text $ Task.taskPToString task
                    D.SProgress p ->
                        case p of
                            ToComplete { done } -> pure $ D.text $ if done then "[X]" else "[ ]"
                            Task taskP -> pure $ D.text $ Task.taskPToString taskP
                            _ -> mempty
                    _ ->
                        mempty
                )
                modRec.fvalue

        convertDecoratorToSuffix :: DecoratorRec -> Maybe (Doc Unit)
        convertDecoratorToSuffix modRec =
            DH.withImpl @D.Decorator -- TODO: export tags to strings beforehand
                mempty
                (case _ of
                    D.SProgress p ->
                        _progressSuffixOneLiner p >>= \prg_item -> Just $ D.text ":" <+> prg_item
                    D.SEarnedAt ea ->
                        Just $ D.text " at " <> orgDate (CT.dateToRec ea)
                    D.SDescription desc ->
                        Just $ D.text "/ " <> D.text desc <> D.text " /"
                    D.SReference _ ->
                        Nothing
                    _ -> mempty
                )
                modRec.fvalue

        convertTagsToSuffix :: Array RawTag -> Maybe (Doc Unit)
        convertTagsToSuffix rawTags =
            case rawTags of
                [] -> Nothing
                tagArr ->
                    Just $ D.text " #" <>
                    ( joinWith (D.space <> D.text "#")
                            $ D.text
                        <$> String.replaceAll (String.Pattern " ") (String.Replacement "-")
                        <$> MbW.toString
                        <$> CT.loadRawId
                        -- <$> tagContent
                        <$> tagArr
                    )


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


_progressProperties :: Progress -> Array ( String /\ Doc Unit )
_progressProperties = case _ of
    None -> mempty
    Unknown -> mempty
    PInt i -> pure $ "INTVAL" /\ D.text (show i)
    PNumber n -> pure $ "NUMVAL" /\ D.text (show n)
    PText text -> pure $ "TEXTVAL" /\ D.text text
    ToComplete { done } -> pure $ "TOCOMPLETE" /\ D.text (if done then "true" else "false")
    PercentI i -> pure $ "PERCENTI" /\ D.text (show i)
    PercentN n -> pure $ "PERCENTN" /\ D.text (show n)
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ "PERCENTX" /\ D.text (sign_s <> " " <> show pct)
    ToGetI { got, total } -> pure $
        "TOGETI" /\ D.text (show got <> " " <> show total)
    ToGetN { got, total } -> pure $
        "TOGETN" /\ D.text (show got <> " " <> show total)
    OnTime timeRec ->
        pure $ "ONTIME" /\ orgTime timeRec
    OnDate sdate ->
        pure $ "ONDATE" /\ orgDate (CT.dateToRec sdate)
    PerI { amount, per } ->
        pure $ "PERI" /\ (D.text (show amount) <> D.text " " <> D.text per)
    PerN { amount, per } ->
        pure $ "PERN" /\ (D.text (show amount) <> D.text " " <> D.text per)
    MeasuredI { amount, measure } ->
        pure $ "MESI" /\ (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredN { amount, measure } ->
        pure $ "MESN" /\ (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ "MESX" /\ (D.text sign_s <> D.text " " <> D.text (show amount) <> D.text "    " <> D.text measure)
    RangeI { from, to } ->
        pure $ "RANGEI" /\ D.text (show from <> " " <> show to)
    RangeN { from, to } ->
        pure $ "RANGEN" /\ D.text (show from <> " " <> show to)
    Task taskP ->
        pure $ "TASK" /\ D.text (Task.taskPToString taskP)
    LevelsI { reached, levels } ->
        mempty -- TODO
    LevelsN { reached, levels } ->
        mempty -- TODO
    LevelsS { reached, levels } ->
        mempty -- TODO
    LevelsE levelsE ->
        pure $ "LEVELSE" /\ D.text (show levelsE.reached <> " " <> show levelsE.total)
    LevelsP { levels } ->
        mempty -- TODO
    LevelsC levelsC ->
        [ "LEVELSC_REACHED" /\ D.text (show levelsC.levelReached)
        , "LEVELSC_CURRENT" /\ D.text (show levelsC.reachedAtCurrent)
        , "LEVELSC_TOTAL" /\ D.text (show levelsC.totalLevels)
        , "LEVELSC_MAXCURRENT" /\ D.text (show levelsC.maximumAtCurrent)
        ]
    LevelsO { reached, levels } ->
        mempty -- TODO
    RelTime rel timeRec ->
        let relText =
                case rel of
                    RMoreThan -> "more_than"
                    REqual -> "equal"
                    RLessThan -> "less_than"
        in pure $ "RELTIME" /\ (D.text relText <> D.space <> orgTime timeRec)
    Error err ->
        mempty



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