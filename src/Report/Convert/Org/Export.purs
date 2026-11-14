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

import Report.Decorator (Key(..), Decorator(..), keyOf) as D
import Report.AtomicValue (AtomicValue(..))
import Report.Decorators.Progress (Progress(..), PValueTag(..), Levels(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (Tags, RawTag(..))
import Data.Array.NonEmpty (toArray) as NEA
import Report.Decorators.Progress (Progress(..)) as P
import Report.Tabular (Tabular)
import Report.Tabular (findV, items, Item(..)) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.AtomicValue as AV
import Report.AtomicValue.Marker.Org (toOrgMarker, toOrgDecoratorMarker) as OrgMarker
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
            TV.TVAtomic (AV.OnDate sdate) -> Just $ CT.dateToRec sdate
            TV.TVAtomic ((OnDate sdate)) -> Just $ CT.dateToRec sdate
            _ -> Nothing
        -- mbPlaytime tabular = Tabular.findV "playtime" tabular >>= case _ of
        --     AV.OnTime timeRec -> Just timeRec
        --     _ -> Nothing

        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Doc Unit
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            D.text "* " <> D.text subjectRec.name
            <> D.break <> propertiesBlock
                (Array.catMaybes
                    [ Just $ "Id" /\ D.text (unwrap subjectRec.id)
                    , Just $ "Platform" /\ D.text "FOFO"
                    , Just $ "Playtime" /\ D.text "FOFO"
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

        convertGroup :: Int -> { group :: Group, items :: Array ItemRec } -> Doc Unit
        convertGroup index { group, items } =
            let
                groupRec = unwrap group
                groupHeadingPrefix = makeHeading $ GroupPath.howDeep groupRec.path + 1
            in
            groupHeadingPrefix <+> D.text groupRec.title
            <> D.break <> propertiesBlock
                (  [ "Path" /\ orgPath groupRec.path
                   , "Index" /\ D.text (show index)
                   ]
                <> _tabularProperties groupRec.tabular
                )
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
                _decoratorProperties
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
                            AV (ToComplete { done }) -> pure $ D.text $ if done then "[X]" else "[ ]"
                            AV (PTask taskP) -> pure $ D.text $ Task.taskPToString taskP
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


orgPath :: GroupPath -> Doc Unit
orgPath path =
    joinWith D.space $ D.text <$> (unwrap <$> unwrap path)


orgTime :: CT.STimeRec -> Doc Unit
orgTime timeRec
    =  D.text (CT.toLeadingZero timeRec.hrs)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.min)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.sec)


-- | the property names are the markers from `Report.AtomicValue.Marker.Org`
_decoratorProperties :: D.Decorator -> Array ( String /\ Doc Unit )
_decoratorProperties dec =
    Array.fromFoldable $ (\doc -> OrgMarker.toOrgDecoratorMarker (D.keyOf dec) /\ doc) <$> _decoratorPropertyValue dec


_decoratorPropertyValue :: D.Decorator -> Maybe (Doc Unit)
_decoratorPropertyValue = case _ of
    D.PRating rating ->
        pure $ D.text (show (Rating.toNumber rating) <> " " <> show (Rating.maxValue rating))
    D.PPriority priority ->
        pure $ D.text $ Priority.priorityChar priority
    D.PTask task ->
        pure $ D.text $ Task.taskPToString task
    D.SProgress p ->
        _progressPropertyValue p
    D.SEarnedAt ea ->
        pure $ orgDate (CT.dateToRec ea)
    D.SDescription desc ->
        pure $ D.text desc
    D.SReference path ->
        pure $ orgPath path
    D.SJoinReference path ->
        pure $ orgPath path


-- | the property names are the markers from `Report.AtomicValue.Marker.Org`
_atomicProperties :: AtomicValue -> Array ( String /\ Doc Unit )
_atomicProperties aval =
    Array.fromFoldable $ (\doc -> OrgMarker.toOrgMarker (AV.valueTagOf aval) /\ doc) <$> _atomicPropertyValue aval


_atomicPropertyValue :: AtomicValue -> Maybe (Doc Unit)
_atomicPropertyValue = case _ of
    None -> mempty
    Unknown -> mempty
    PInt i -> pure $ D.text (show i)
    PNumber n -> pure $ D.text (show n)
    PText text -> pure $ D.text text
    ToComplete { done } -> pure $ D.text (if done then "true" else "false")
    PercentI i -> pure $ D.text (show i)
    PercentN n -> pure $ D.text (show n)
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ D.text (sign_s <> " " <> show pct)
    ToGetI { got, total } -> pure $
        D.text (show got <> " " <> show total)
    ToGetN { got, total } -> pure $
        D.text (show got <> " " <> show total)
    OnTime timeRec ->
        pure $ orgTime timeRec
    OnDate sdate ->
        pure $ orgDate (CT.dateToRec sdate)
    PerI { amount, per } ->
        pure $ (D.text (show amount) <> D.text " " <> D.text per)
    PerN { amount, per } ->
        pure $ (D.text (show amount) <> D.text " " <> D.text per)
    MeasuredI { amount, measure } ->
        pure $ (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredN { amount, measure } ->
        pure $ (D.text (show amount) <> D.text " " <> D.text measure)
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ (D.text sign_s <> D.text " " <> D.text (show amount) <> D.text " " <> D.text measure)
    RangeI { from, to } ->
        pure $ D.text (show from <> " " <> show to)
    RangeN { from, to } ->
        pure $ D.text (show from <> " " <> show to)
    PTask taskP ->
        pure $ D.text (Task.taskPToString taskP)
    RelTime rel timeRec ->
        pure $ (D.text (orgRelation rel) <> D.space <> orgTime timeRec)
    Error err ->
        pure $ D.text err
    PBoolean b -> pure $ D.text (if b then "true" else "false")
    PYear year -> pure $ D.text (show year)
    PID id -> pure $ D.text (show id)
    ToGetIX { got, doing, total } -> pure $
        D.text (show got <> " " <> show doing <> " " <> show total)
    ToGetNX { got, doing, total } -> pure $
        D.text (show got <> " " <> show doing <> " " <> show total)
    ToGetMI { min, got, max } -> pure $
        D.text (show min <> " " <> show got <> " " <> show max)
    ToGetMN { min, got, max } -> pure $
        D.text (show min <> " " <> show got <> " " <> show max)
    ToGetMIX { min, got, doing, max } -> pure $
        D.text (show min <> " " <> show got <> " " <> show doing <> " " <> show max)
    ToGetMNX { min, got, doing, max } -> pure $
        D.text (show min <> " " <> show got <> " " <> show doing <> " " <> show max)
    OnDateTime sdate timeRec ->
        pure $ (orgDate (CT.dateToRec sdate) <> D.space <> orgTime timeRec)
    TimeRange { from, to } ->
        pure $ (orgTime from <> D.text "--" <> orgTime to)
    DateRange { from, to } ->
        pure $ (orgDate (CT.dateToRec from) <> D.text "--" <> orgDate (CT.dateToRec to))
    DateTimeRange { from, to } ->
        pure $ 
            (  orgDate (CT.dateToRec from.date) <> D.space <> orgTime from.time
            <> D.text "--"
            <> orgDate (CT.dateToRec to.date) <> D.space <> orgTime to.time
            )
    RelInt rel int ->
        pure $ (D.text (orgRelation rel) <> D.space <> D.text (show int))
    RelNum rel num ->
        pure $ (D.text (orgRelation rel) <> D.space <> D.text (show num))
    PPath path ->
        pure $ D.text (MbW.toStringSep "::" path)
    Tags tags ->
        pure $ D.text (String.joinWith "," $ MbW.toStringSep "::" <$> tags)
    PRawTags rawTags ->
        pure $ D.text (String.joinWith "," $ orgRawTag <$> unwrap rawTags)
    PColor color ->
        pure $ D.text (AV.toReadableString $ PColor color)
    PRating rating ->
        pure $ D.text (show (Rating.toNumber rating) <> " " <> show (Rating.maxValue rating))
    PPriority priority ->
        pure $ D.text (Priority.priorityChar priority)


-- | Only the atomic tabular values are written, since the properties are one-liners;
-- | the property name is the key of the tabular value with the `RTB_` prefix, so that it never mixes with the core org properties
-- | and the other properties written by the export.
_tabularProperties :: Tabular TV.TabularValue -> Array ( String /\ Doc Unit )
_tabularProperties =
    Tabular.items >>> Array.mapMaybe \(Tabular.Item { key, value }) -> case value of
        TV.TVAtomic aval -> (\doc -> tabularPropertyName key /\ doc) <$> _atomicPropertyValue aval
        _ -> Nothing


tabularPropertyName :: String -> String
tabularPropertyName key = "RTB_" <> key


orgRelation :: CT.Relation -> String
orgRelation = case _ of
    CT.RMoreThan -> "more_than"
    CT.REqual -> "equal"
    CT.RLessThan -> "less_than"


orgRawTag :: RawTag -> String
orgRawTag (RawTag { id, content }) =
    String.joinWith "::" (NEA.toArray id) <> " // " <> String.joinWith "::" (NEA.toArray content)


_levelsPropertyValue :: Levels -> Maybe (Doc Unit)
_levelsPropertyValue = case _ of
    LevelsI { reached, levels } ->
        mempty -- TODO
    LevelsN { reached, levels } ->
        mempty -- TODO
    LevelsS { reached, levels } ->
        mempty -- TODO
    LevelsE levelsE ->
        pure $ D.text (show levelsE.reached <> " " <> show levelsE.total)
    LevelsC levelsC ->
        pure $ D.text
            (  show levelsC.levelReached
            <> " " <> show levelsC.reachedAtCurrent
            <> " " <> show levelsC.totalLevels
            <> " " <> show levelsC.maximumAtCurrent
            )
    LevelsP { levels } ->
        mempty -- TODO
    LevelsO { reached, levels } ->
        mempty -- TODO


_progressPropertyValue :: Progress -> Maybe (Doc Unit)
_progressPropertyValue = case _ of
    AV aval -> _atomicPropertyValue aval
    LS levels -> _levelsPropertyValue levels


_progressProperties :: Progress -> Array ( String /\ Doc Unit )
_progressProperties = D.SProgress >>> _decoratorProperties



_atomicSuffixOneLiner :: AtomicValue -> Maybe (Doc Unit)
_atomicSuffixOneLiner = case _ of
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
    PTask taskP ->
        mempty -- FIXME: goes as prefix
    RelTime rel timeRec ->
        let relText =
                case rel of
                    CT.RMoreThan -> ">"
                    CT.REqual -> "="
                    CT.RLessThan -> "<"
        in pure $ D.text relText <> D.space <> orgTime timeRec
    Error err ->
        mempty
    OnDateTime sdate timeRec ->
        pure $ orgDate (CT.dateToRec sdate) <> D.space <> orgTime timeRec
    TimeRange { from, to } ->
        pure $ orgTime from <> D.text "--" <> orgTime to
    DateRange { from, to } ->
        pure $ orgDate (CT.dateToRec from) <> D.text "--" <> orgDate (CT.dateToRec to)
    DateTimeRange { from, to } ->
        pure
            $  orgDate (CT.dateToRec from.date) <> D.space <> orgTime from.time
            <> D.text "--"
            <> orgDate (CT.dateToRec to.date) <> D.space <> orgTime to.time
    RelInt rel int -> pure $ D.text (CT.encodeRel rel) <> D.space <> D.text (show int)
    RelNum rel num -> pure $ D.text (CT.encodeRel rel) <> D.space <> D.text (show num)
    PRating rating ->
        pure $ D.text $ Rating.toStars rating
    PPriority priority ->
        mempty -- goes as prefix
    aval ->
        pure $ D.text $ AV.toReadableString aval


_levelsSuffixOneLiner :: Levels -> Maybe (Doc Unit)
_levelsSuffixOneLiner = case _ of
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


_progressSuffixOneLiner :: Progress -> Maybe (Doc Unit)
_progressSuffixOneLiner = case _ of
    AV aval -> _atomicSuffixOneLiner aval
    LS levels -> _levelsSuffixOneLiner levels