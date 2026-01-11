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
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag, tagContent)
import Report.Convert.Keyed (class EncodableKey, decodeKey)
import Report.Convert.Types
import Report.Convert.Generic (class ToExport, toExport, IncludeRule) as Report

import Report.Modifiers.Progress (Progress(..), PValueTag(..), Relation(..))
import Report.Modifiers.Task (TaskP(..))
import Report.Modifiers.Tags (Tags)
import Report.Prefix (Key(..), Prefix(..)) as P
import Report.Suffix (Key(..), Suffix(..)) as S
import Report.Modifiers.Progress (Progress(..)) as P
import Report.Tabular (Tabular)
import Report.Tabular (findV) as Tabular
import Report.Modifiers.Tabular.TabularValue as TV

import Dodo
import Dodo as D


toOrg
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . Report.ToExport subj_id subj_tag item_tag subj group item x
    => ReadForeign item_tag
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
            TV.TVDate sdate -> Just $ CT.dateToRec sdate
            TV.TVSuffix (S.SProgress (P.OnDate sdate)) -> Just $ CT.dateToRec sdate
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
            <> D.break <> fold (mapWithIndex convertGroup groups)

        orgDate :: CT.SDateRec -> Doc Unit
        orgDate dateRec
            =  D.text "<" <> D.text (show dateRec.year)
            <> D.text "-" <> D.text (CT.toLeadingZero dateRec.mon)
            <> D.text "-" <> D.text (CT.toLeadingZero dateRec.day)
            <> D.text ">"

        propertiesBlock :: Array (String /\ Doc Unit) -> Doc Unit
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
            <> (joinWith D.break $ arrayD $ convertItem groupRec.path <$> items)

        convertItem :: GroupPath -> ItemRec -> Doc Unit
        convertItem grpPath itemRec =
            let itemHeadingPrefix = makeHeading $ GroupPath.howDeep grpPath + 2 in
            D.text "T.kv_" <> D.space <> quote itemRec.name <> (alignModifiers $ convertModifier <$> itemRec.modifiers)

        convertModifier :: ModifierRec -> RenderedAs (Doc Unit)
        convertModifier modRec =
            case modRec.mkind of
                P ->
                    withImplRA @P.Prefix
                        (case _ of
                            P.PRating rating ->
                                _ol $ D.text "p_rating " -- <> quote (modRec.value)
                            P.PPriority priority ->
                                _ol $ D.text "p_priority " -- <> quote (modRec.value)
                            P.PTask task ->
                                _ol $ D.text "p_task " -- <> quote (modRec.value)
                        )
                        modRec.value
                S ->
                    withImplRA @(S.Suffix item_tag) -- TODO: export tags to strings beforehand
                        (case _ of
                            S.SProgress p ->
                                _progressToDhall p
                            S.SEarnedAt ea ->
                                ea # sdaterec # prefixD "// T.inj/date" # _ol
                            S.SDescription desc ->
                                desc # quote # prefixD "// T.inj/det" # _ol
                            S.SReference path ->
                                unwrap path
                                    # map (unwrap >>> quote)
                                    # ilarrayD
                                    # prefixD "// T.inj/self"
                                    # _ol
                            S.STags tags ->
                                unwrap tags
                                    # map (tagContent >>> quote)
                                    # ilarrayD
                                    # prefixD "// T.inj/tags"
                                    # _ol
                        )
                        modRec.value


        alignModifiers :: Array (RenderedAs (Doc Unit)) -> Doc Unit
        alignModifiers renderedAs =
            case Array.uncons renderedAs of
                Just { head, tail } ->
                    case head of
                        OneLine dhead ->
                            D.space <> dhead <> alignModifiers tail
                        MutliLine dhead ->
                            (fromnl $ D.indent $ joinWith D.break $ dhead) <> alignModifiers tail
                Nothing ->
                    mempty


withImpl :: forall @t x. ReadForeign t => x -> (t -> x) -> Foreign -> x
withImpl err f frgn = (readImpl frgn :: F t) # runExcept # either (const err) f


withImplD :: forall @t a. ReadForeign t => (t -> Doc a) -> Foreign -> Doc a
withImplD = withImpl $ D.text "{- <error> -}"


withImplRA :: forall @t a. ReadForeign t => (t -> RenderedAs (Doc a)) -> Foreign -> RenderedAs (Doc a)
withImplRA = withImpl $ _ol $ D.text "{- <error> -}"


brindent2 :: forall a. Array (Doc a) -> Doc a
brindent2 = map (\d -> D.break <> D.indent (D.indent d)) >>> fold


data RenderedAs a
    = OneLine a
    | MutliLine (Array a)


_unwrapRA :: forall a. RenderedAs a -> Array a
_unwrapRA = case _ of
    OneLine a -> pure a
    MutliLine arr -> arr


_ol :: forall a. Doc a -> RenderedAs (Doc a)
_ol = OneLine


_ml :: forall a. Array (Doc a) -> RenderedAs (Doc a)
_ml = MutliLine


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
        {-
        let dateRec = CT.dateToRec sdate
        in pure $ "T.v_date " <> wrecord
            [ "day"  /\ ("+" <> show dateRec.day)
            , "mon"  /\ ("+" <> show dateRec.mon)
            , "year" /\ ("+" <> show dateRec.year)
            ]
        -}
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


ilarray :: forall a. Array String -> Doc a
ilarray = ilarrayD <<< map D.text


ilarrayD :: forall a. Array (Doc a) -> Doc a
ilarrayD = wraparrD <<< joinWith (D.text "," <> D.space)


array :: forall a. Array String -> Array (Doc a)
array = map D.text >>> arrayD


arrayD :: forall a. Array (Doc a) -> Array (Doc a)
arrayD items =
    mpi (\i d -> if i == 0 then D.text "[" <+> d else D.text "," <+> d) items <> [ D.text "]" ]


quote :: forall a. String -> Doc a
quote = D.text >>> quoteD

quoteD :: forall a. Doc a -> Doc a
quoteD = D.enclose (D.text "\"") (D.text "\"")

wrapbrk :: forall a. String -> Doc a
wrapbrk = D.text >>> wrapbrkD

wrapbrkD :: forall a. Doc a -> Doc a
wrapbrkD = D.enclose (D.text "(") (D.text ")")


-- wraparr :: forall a. String -> Doc a
-- wraparr = D.text >>> wraparrD


wraparrD :: forall a. Doc a -> Doc a
wraparrD = D.enclose (D.text "[ ") (D.text " ]")


prefix :: forall a. String -> String -> Doc a
prefix p = D.text >>> prefixD p


prefixD :: forall a. String -> Doc a -> Doc a
prefixD p s = D.text p <+> s


fromnl :: forall a. Doc a -> Doc a
fromnl d = D.break <> d


prefixnosp :: forall a. String -> String -> Doc a
prefixnosp p s = D.text p <> D.text s


field :: forall a. (String /\ String) -> Doc a
field = Tuple.uncurry $ \n v -> D.text n <+> D.text "=" <+> D.text v


fieldD :: forall a. (String /\ Doc a) -> Doc a
fieldD = Tuple.uncurry $ \n docv -> D.text n <+> D.text "=" <+> docv


joinWith :: forall a. Doc a -> Array (Doc a) -> Doc a
joinWith sep =
    fold <<< mpix \d -> sep <> d


joinWithSp :: forall a. Doc a -> Array (Doc a) -> Doc a
joinWithSp sep =
    fold <<< mpix \d -> sep <+> d


mpix :: forall a. (Doc a -> Doc a) -> Array (Doc a) -> Array (Doc a)
mpix f = mpi $ \i d -> if i == 0 then d else f d


mpi :: forall a. (Int -> Doc a -> Doc a) -> Array (Doc a) -> Array (Doc a)
mpi = mapWithIndex


wrecord :: forall a. Array (String /\ String) -> Doc a
wrecord fields =
    D.enclose (D.text "{ ") (D.text " }") $
        joinWith (D.text "," <> D.space) $ field <$> fields


wrecordD :: forall a. Array (String /\ Doc a) -> Doc a
wrecordD fields =
    D.enclose (D.text "{ ") (D.text " }") $
        joinWith (D.text "," <> D.space) $ fieldD <$> fields


wrecordDWithDate :: forall a. Maybe CT.SDateRec -> Array (String /\ Doc a) -> Doc a
wrecordDWithDate mbDateRec fields =
    case mbDateRec of
        Just _ ->
            wrecordD
                $ fields <>
                [ "date" /\ mbdaterec mbDateRec ]
        Nothing ->
            wrecordD fields
            <+> D.text "// T.inj/no_date"


daterecf :: forall a. CT.SDateRec -> Doc a
daterecf dateRec =
    wrecord
        [ "day"  /\ ("+" <> show dateRec.day)
        , "mon"  /\ ("+" <> show dateRec.mon)
        , "year" /\ ("+" <> show dateRec.year)
        ]


timereccf :: forall a. CT.STimeRec -> Doc a
timereccf timeRec =
    wrecord
        [ "hrs" /\ ("+" <> show timeRec.hrs)
        , "min" /\ ("+" <> show timeRec.min)
        , "sec" /\ ("+" <> show timeRec.sec)
        ]

sdaterec :: forall a. CT.SDate -> Doc a
sdaterec =
    daterecf <<< CT.dateToRec


mbdaterec :: forall a. Maybe CT.SDateRec -> Doc a
mbdaterec =
    mbgenD (D.text "T.DATE") daterecf


mbgen :: forall x a. String -> (x -> String) -> Maybe x -> Doc a
mbgen t f = mbgenD (D.text t) (D.text <<< f)


mbgenD :: forall x a. Doc a -> (x -> Doc a) -> Maybe x -> Doc a
mbgenD t f = maybe (D.text "None" <+> t) $ \v -> D.text "Some" <+> f v


indent = "    " :: String
indent2 = indent <> indent :: String
indent3 = indent2 <> indent :: String
indent4 = indent3 <> indent :: String
