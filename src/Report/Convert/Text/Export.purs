module Report.Convert.Text.Export where

import Prelude


import Prelude

import Foreign (Foreign, F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe, fromMaybe)
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

import Report.Decorator (Key(..), Decorator(..)) as D
import Report.Decorators.Progress (Progress(..), PValueTag(..), Relation(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Tags (Tags, RawTag)
import Report.Decorators.Progress (Progress(..)) as P
import Report.Tabular (Tabular)
import Report.Tabular (findV, Item(..), items) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.Decorators.Tabular.TabularValue as TV

import Report.Convert.Dhall.Export as DH

import Dodo
import Dodo as D


{-
data Style
    = Tree
    | Flow


type Config = { style :: Style, indent :: String }
-}


toText
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => IsTag item_tag
    => Report.IncludeRule subj_id
    -> Report subj group item
    -> String
toText inclRule =
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
            D.text "#" <> D.space <> D.text subjectRec.name
            <> D.break <> D.indent (_tabularsBlock
                $ Array.catMaybes
                    [ {- Just $ "Id" /\ D.text (unwrap subjectRec.id)
                    , -} mbTrackedAt subjectRec.tabular <#> \dateRec -> "TrackedAt" /\ textDate dateRec
                    ]
                )
            <> D.break <> joinWith D.break (mapWithIndex convertGroup groups)

        addIndent :: Int -> Doc Unit -> Doc Unit
        addIndent level toWhat =
            if level == 0 then toWhat else D.indent $ addIndent (level - 1) toWhat

        convertPath :: GroupPath -> Doc Unit
        convertPath path =
            joinWith (D.text "::") $ D.text <$> (unwrap <$> unwrap path)

        convertGroup :: Int -> { group :: Group, items :: Array ItemRec } -> Doc Unit
        convertGroup index { group, items } =
            let
                groupRec = unwrap group
                groupDeepLevel = GroupPath.howDeep groupRec.path
                groupIndent = addIndent groupDeepLevel
            in
            groupIndent (D.text (show index) <> D.text "." <+> D.text groupRec.title <+> convertPath groupRec.path)
            <> D.break
            <> (joinWith D.break $ convertItem groupRec.path <$> items)
            <> D.break

        convertItem :: GroupPath -> ItemRec -> Doc Unit
        convertItem grpPath itemRec =
            let
                -- groupDeepLevel = GroupPath.howDeep groupRec.path
                groupDeepLevel = GroupPath.howDeep grpPath
                itemIndent = addIndent $ groupDeepLevel + 1
                decoratorsPrefixes   = Array.catMaybes $ convertDecoratorToPrefix   <$> itemRec.decorators
                decoratorsSuffixes   = Array.catMaybes $ convertDecoratorToSuffix   <$> itemRec.decorators
                tabulars             =                   _convertTabularToDoc       <$> itemRec.tabulars
                decoratorsPrefixesDoc =
                    case decoratorsPrefixes of
                        [] -> mempty
                        prefixes -> joinWith D.space prefixes <> D.space
                decoratorsSuffixesDoc =
                    case decoratorsSuffixes of
                        [] -> mempty
                        suffixes -> D.space <> joinWith D.space suffixes
            in
            itemIndent (decoratorsPrefixesDoc <> D.text itemRec.title <> decoratorsSuffixesDoc)
            <> _tabularsBlockDoc tabulars

        convertDecoratorToPrefix :: DecoratorRec -> Maybe (Doc Unit)
        convertDecoratorToPrefix modRec =
            DH.withImpl @(D.Decorator RawTag) -- TODO: export tags to strings beforehand
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
            DH.withImpl @(D.Decorator RawTag) -- TODO: export tags to strings beforehand
                mempty
                (case _ of
                    D.SProgress p ->
                        _progressSuffixOneLiner p >>= \prg_item -> Just $ D.text ":" <+> prg_item
                    D.SEarnedAt ea ->
                        Just $ D.text " at " <> textDate (CT.dateToRec ea)
                    D.SDescription desc ->
                        Just $ D.text "/ " <> D.text desc <> D.text " /"
                    D.SReference _ ->
                        Nothing
                    D.STags theTags ->
                        case unwrap theTags of
                            [] -> Nothing
                            tagArr ->
                                Just $ tags tagArr
                    _ -> mempty
                )
                modRec.fvalue


joinWith :: forall a. Doc a -> Array (Doc a) -> Doc a
joinWith sep =
    fold <<< DH.mpix \d -> sep <> d


textDate :: CT.SDateRec -> Doc Unit
textDate dateRec
    =  D.text (CT.toLeadingZero dateRec.day)
    <> D.text "-" <> D.text (CT.monthThreeLetter $ CT.monthFromInt dateRec.mon)
    <> D.text "-" <> D.text (show dateRec.year)


textTime :: CT.STimeRec -> Doc Unit
textTime timeRec
    =  D.text (CT.toLeadingZero timeRec.hrs)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.min)
    <> D.text ":" <> D.text (CT.toLeadingZero timeRec.sec)


_tabularsBlock :: Array (String /\ Doc Unit) -> Doc Unit
_tabularsBlock [] = mempty
_tabularsBlock fields =
    (joinWith D.break $ _tabularLine <$> fields) <> D.break


_tabularLine :: (String /\ Doc Unit) -> Doc Unit
_tabularLine (name /\ value) = D.text "-" <> D.space <> D.text name <> D.text ": " <> value


_tabularsBlockDoc :: Array (String /\ Doc Unit) -> Doc Unit
_tabularsBlockDoc tabulars =
    case tabulars of
        [] -> mempty
        _ -> D.break <> _tabularsBlock tabulars


tags :: Array RawTag -> Doc Unit
tags tagArr = D.text " #" <>
    ( joinWith (D.space <> D.text "#")
            $ D.text
        <$> String.replaceAll (String.Pattern " ") (String.Replacement "-")
        <$> MbW.toString
        <$> tagContent
        <$> tagArr
    )


_convertTabularToDoc :: TabularRec -> String /\ Doc Unit
_convertTabularToDoc { tkey, tlabel, value } = tlabel /\ _tabularValueDoc value


_collectTabularItem :: Tabular.Item TV.TabularValue -> TabularRec
_collectTabularItem = case _ of
    Tabular.Item { key, label, value } ->
        { tkey  : key
        , tlabel : label
        , value : value -- ValueModify.toEditable
        }


_tabularValueDoc :: TV.TabularValue -> Doc Unit
_tabularValueDoc = case _ of
    TV.TVAtomic atomicValue -> _tabularAtomicValueDoc atomicValue
    TV.TVValues aValues -> joinWith (D.break <> D.text "- ") $ _tabularAtomicValueDoc <$> aValues
    TV.TVValuesNest aValuesNest -> joinWith D.break $ (joinWith $ D.break <> D.break) <$> map _tabularAtomicValueDoc <$> aValuesNest
    TV.TVTabulars tabulars ->
        joinWith (D.break <> D.break)
             $ (Tabular.items
                >>> map (map TV.TVAtomic >>> _collectTabularItem >>> _convertTabularToDoc)
                >>> _tabularsBlock)
            <$> tabulars
    TV.TVPair left right -> _tabularValueDoc left <> D.text " | " <> _tabularValueDoc right
    TV.TVTabularsNest tabularsNest -> mempty -- TODO: implement -- joinWith D.break $ ?wh <$> tabularsNest


_tabularAtomicValueDoc :: TV.TabularAtomicValue -> Doc Unit
_tabularAtomicValueDoc = case _ of
    TV.TVString str -> D.text str
    TV.TVNumber num -> D.text $ show num
    TV.TVBoolean b -> D.text $ show b
    TV.TVInt i -> D.text $ show i
    TV.TVDate dateRec -> textDate $ CT.dateToRec dateRec
    TV.TVTime timeRec -> textTime timeRec
    TV.TVYear year -> D.text $ show year
    TV.TVID n -> D.text $ show n
    TV.TVDateTime dateRec timeRec -> textDate (CT.dateToRec dateRec) <> D.text " " <> textTime timeRec
    TV.TVTimeRange { from, to } -> textTime from <> D.text "--" <> textTime to
    TV.TVDateRange { from, to } -> textDate (CT.dateToRec from) <> D.text "--" <> textDate (CT.dateToRec to)
    TV.TVDateTimeRange { from, to } -> textDate (CT.dateToRec from.date) <> D.text " " <> textTime from.time <> D.text "--" <> textDate (CT.dateToRec to.date) <> D.text " " <> textTime to.time
    TV.TVDecorator decorator -> case decorator of
        D.SProgress p -> fromMaybe mempty $ _progressSuffixOneLiner p
        D.SEarnedAt ea -> D.text " at " <> textDate (CT.dateToRec ea)
        D.SDescription desc -> D.text "/ " <> D.text desc <> D.text " /"
        D.SReference _ -> mempty
        D.STags theTags -> case unwrap theTags of
            [] -> mempty
            tagArr ->
                tags tagArr
        D.PRating rating -> D.text $ show $ Rating.toStars rating
        D.PPriority priority -> D.text "[#" <> D.text (Priority.priorityChar priority) <> D.text "]"
        D.PTask task -> D.text $ Task.taskPToString task


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
        pure $ textTime timeRec
    OnDate sdate ->
        pure $ textDate (CT.dateToRec sdate)
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
        in pure $ D.text relText <> D.space <> textTime timeRec
    Error err ->
        mempty