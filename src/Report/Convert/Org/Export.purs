module Report.Convert.Org.Export where

import Prelude

import Foreign (Foreign, F)
import Control.Monad.Except (runExcept)

import Data.Maybe (Maybe(..), maybe)
import Data.Either (either)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (uncurry) as Tuple
import Data.String (joinWith) as String
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
import Report.Tabular (findV) as Tabular
import Report.Decorators.Rating as Rating
import Report.Decorators.Priority as Priority
import Report.Decorators.Task as Task
import Report.Decorators.Tabular.TabularValue as TV

import Report.Convert.Dhall.Export as DH

import Dodo
import Dodo as D


toOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => IsTag item_tag
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
            D.text "* " <> D.text subjectRec.name
            <> D.break <> propertiesBlock
                (Array.catMaybes
                    [ Just $ "Id" /\ D.text (unwrap subjectRec.id)
                    , Just $ "Platform" /\ D.text "TODO"
                    , Just $ "Playtime" /\ D.text "TODO"
                    , mbTrackedAt subjectRec.tabular >>= \dateRec ->
                        Just $ "TrackedAt" /\ orgDate dateRec
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
                        [] -> mempty
                        props -> D.break <> propertiesBlock ( ("Name" /\ D.text itemRec.name) : props )
            in
            itemHeadingPrefix <+> decoratorsPrefixesDoc <> D.text itemRec.name <> decoratorsSuffixesDoc
            <> propertiesBlockDoc

        convertDecoratorToProperty :: DecoratorRec -> Array (String /\ Doc Unit)
        convertDecoratorToProperty modRec =
            DH.withImpl @(D.Decorator RawTag) -- TODO: export tags to strings beforehand
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
                    D.STags tags ->
                        pure $ "Tags" /\ DH.joinWith D.space (D.text <$> MbW.toString <$> tagContent <$> unwrap tags)
                )
                modRec.value

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
                modRec.value

        convertDecoratorToSuffix :: DecoratorRec -> Maybe (Doc Unit)
        convertDecoratorToSuffix modRec =
            DH.withImpl @(D.Decorator RawTag) -- TODO: export tags to strings beforehand
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
                    D.STags tags ->
                        case unwrap tags of
                            [] -> Nothing
                            tagArr ->
                                Just $ D.text " #" <> (joinWith (D.space <> D.text "#") $ D.text <$> MbW.toString <$> tagContent <$> tagArr)
                    _ -> mempty
                )
                modRec.value


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



{-
_progressToDhall :: Progress -> RenderedAs (Doc Unit)
_progressToDhall = case _ of
    None ->        _ol $ D.text "T.v_empty"
    Unknown ->     _ol $ D.text "T.v_unk"
    PInt i ->      _ol $ wrapbrkD $ D.text "T.v_i" <+> (prefixnosp "+" $ show i)
    PNumber n ->   _ol $ wrapbrkD $ D.text "T.v_n" <+> D.text (show n)
    PText text ->  _ol $ wrapbrkD $ D.text "T.v_t" <+> quote text
    ToComplete { done } -> _ol $ wrapbrkD $ D.text $ if done then "T.v_done" else "T.v_none"
    PercentI i ->  _ol $ wrapbrkD $ D.text "T.v_pcti" <+> (prefixnosp "+" $ show i)
    PercentN n ->  _ol $ wrapbrkD $ D.text "T.v_pct" <+> D.text (show n)
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in _ol $ wrapbrkD $ D.text "T.v_pctx" <+> D.text sign_s <+> D.text (show pct)
    ToGetI { got, total } -> _ol $
        if (got == 0) && (total == 1) then
            wrapbrkD $ D.text "T.v_none_"
        else if (got == 1) && (total == 1) then
            wrapbrkD $ D.text "T.v_done_"
        else if (got == 0) && (total == 0) then
            wrapbrkD $ D.text "T.v_vone_"
        else
            wrapbrkD $ D.text "T.v_pi " <> wrecord
                [ "done" /\ ("+" <> show got)
                , "total" /\ ("+" <> show total)
                ]
    ToGetN { got, total } ->
        _ol $ wrapbrkD $ D.text "T.v_pd " <> wrecord
            [ "got" /\ show got
            , "total" /\ show total
            ]
    OnTime timeRec ->
        _ol $ wrapbrkD $ D.text "T.v_time " <> timereccf timeRec
    OnDate sdate ->
        _ol $ wrapbrkD $ D.text "T.v_date " <> sdaterec sdate
    PerI { amount, per } ->
        _ol $ wrapbrkD $ D.text "T.v_per" <+> (prefixnosp "+" $ show amount) <+> quote per
    PerN { amount, per } ->
        _ol $ wrapbrkD $ D.text "T.v_per" <+> D.text (show amount) <+> quote per
    MeasuredI { amount, measure } ->
        _ol $ wrapbrkD $ D.text "T.v_mes" <+> (prefixnosp "+" $ show amount) <+> quote measure
    MeasuredN { amount, measure } ->
        _ol $ wrapbrkD $ D.text "T.v_mesd" <+> D.text (show amount) <+> quote measure
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in _ol $ wrapbrkD $ D.text "T.v_mesx" <+> D.text sign_s <+> (prefixnosp "+" $ show amount) <+> quote measure
    RangeI { from, to } ->
        _ol $ wrapbrkD $ D.text "T.v_rng " <> wrecord
            [ "from" /\ ("+" <> show from)
            , "to" /\ ("+" <> show to)
            ]
    RangeN { from, to } ->
        _ol $ wrapbrkD $ D.text "T.v_rngd " <> wrecord
            [ "from" /\ show from
            , "to" /\ show to
            ]
    Task taskP ->
        _ol $ wrapbrkD $ D.text "T.v_proc " <> quoteD (vtaskP taskP)
    LevelsI { reached, levels } ->
        let
            levelrec lrec =
                wrecordDWithDate lrec.date
                    [ "maximum" /\ (prefixnosp "+" $ show lrec.maximum)
                    , "name" /\ quote lrec.name
                    ]
        in
        _ml $
            [ D.text "(" <+> D.text "T.v_lvli"
            , D.indent $ sfield $ "reached" /\ ("+" <> show reached)
            , D.indent $ xfield "levels"
            ]
            <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
            [ D.indent recend
            , D.text ")"
            ]
    LevelsN { reached, levels } ->
        let
            levelrec lrec =
                wrecordDWithDate lrec.date
                    [ "maximum" /\ D.text (show lrec.maximum)
                    , "name" /\ quote lrec.name
                    ]
        in
        _ml $
            [ D.text "(" <+> D.text "T.v_lvld"
            , D.indent $ sfield $ "reached" /\ show reached
            , D.indent $ xfield "levels"
            ]
            <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
            [ D.indent recend
            , D.text ")"
            ]
    LevelsS { reached, levels } ->
        let
            levelrec lrec =
                 wrecordDWithDate lrec.date
                    [ "gives" /\ quote lrec.gives
                    ]
        in
        _ml $
            [ D.text "(" <+> D.text "T.v_lvls"
            , D.indent $ sfield $ "reached" /\ ("+" <> show reached)
            , D.indent $ xfield "levels"
            ]
            <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
            [ D.indent recend
            , D.text ")"
            ]
    LevelsE levelsE ->
        _ml $
            [ D.text "(" <+> D.text "T.v_lvlio"
            , wrecord
                [ "reached" /\ ("+" <> show levelsE.reached)
                , "total" /\ ("+" <> show levelsE.total)
                ]
            , D.text ")"
            ]
    LevelsP { levels } ->
        let
            levelrec lrec =
                wrecordDWithDate lrec.date
                    [ "name" /\ quote lrec.name
                    , "proc" /\ pvptaskP lrec.proc
                    ]
        in
        _ml $
            [ D.text "(" <+> D.text "T.v_lvlp"
            , D.indent $ xfield "levels"
            ]
            <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
            [ D.indent recend
            , D.text ")"
            ]
    LevelsC levelsC ->
        _ol $ wrapbrkD $ D.text "T.v_lvlc" <+>
            wrecordDWithDate levelsC.date
                [ "reached" /\ (prefixnosp "+" $ show levelsC.levelReached)
                , "current" /\ (prefixnosp "+" $ show levelsC.reachedAtCurrent)
                , "total" /\ (prefixnosp "+" $ show levelsC.totalLevels)
                , "maxcurrent" /\ (prefixnosp "+" $ show levelsC.maximumAtCurrent)
                ]
    LevelsO { reached, levels } ->
        let
            levelrec lrec =
                wrecordDWithDate lrec.date
                    [ "maximum" /\ mbgenD (D.text "Integer") (\v -> prefixnosp "+" $ show v) lrec.mbMaximum
                    , "name" /\ quote lrec.name
                    ]
        in
        _ml $
            [ D.text "(" <+> D.text "T.v_lvli"
            , D.indent $ sfield $ "reached" /\ ("+" <> show reached)
            , D.indent $ xfield "levels"
            ]
            <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
            [ D.indent recend
            , D.text ")"
            ]
    RelTime rel timeRec ->
        let relText =
                case rel of
                    RMoreThan -> "T.rel_more_than"
                    REqual -> "T.rel_equal"
                    RLessThan -> "T.rel_less_than"
        in _ol $ wrapbrkD $ D.text "T.v_relt" <+> (wrapbrkD $ D.text "T.v_rel_" <> D.text relText <+> D.text "T.TIME" <+> timereccf timeRec)
        -- T.v_relt (T.rel_more_than T.TIME time)
    Error err ->
        _ol $ D.text "ERR" -- FIXME:
    where
        sfield p = D.text "{" <+> field p
        nfield p = D.text "," <+> field p
        xfield n = D.text "," <+> D.text n <+> D.text "="
        sxfield n = D.text "{" <+> D.text n <+> D.text "="
        recend = D.text "}"
        vtaskP = D.text <<< case _ of
            TTodo -> "T.p_todo"
            TDoing -> "T.p_doing"
            TDone -> "T.p_done"
            TWait -> "T.p_wait"
            TNow -> "T.p_now"
            TCanceled -> "T.p_canceled"
            TLater -> "T.p_later"
        pvptaskP task = vtaskP task <> D.text "_"
-}