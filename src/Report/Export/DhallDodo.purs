module Report.Export.DhallDodo where

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
import Data.Array as Array
import Data.Foldable (fold)
import Data.FunctorWithIndex (mapWithIndex)

import Yoga.JSON (class WriteForeign, writePrettyJSON, class ReadForeign, readImpl)

import Report (Report)
import Report.Core as CT
import Report.Group (Group)
import Report.GroupPath (GroupPath)
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag, tagContent)
import Report.Modifiers.Class.ValueModify (class EncodableKey, decodeKey)
import Report.Export.Types
import Report.Export.Generic (toExport) as Report

import Report.Modifiers.Progress (Progress(..), PValueTag(..))
import Report.Modifiers.Task (TaskP(..))
import Report.Modifiers.Tags (Tags)
import Report.Prefix (Key(..)) as P
import Report.Suffix (Key(..)) as S

import Dodo
import Dodo as D


toDhall
    :: forall @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => EncodableKey subj_id
    => WriteForeign item_tag
    => ReadForeign item_tag
    => IsTag subj_tag
    => IsTag item_tag
    => IsItem item_tag item
    => IsGroup group
    => IsSubject subj_id subj_tag subj
    => Report subj group item
    -> String
toDhall =
    Report.toExport @subj_id @subj_tag @item_tag
        >>> unwrap
        >>> _.subjects
        >>> map (unwrap >>> convertSubject)
        >>> D.lines
        >>> D.print D.plainText D.fourSpaces
        -- >>> Array.intersperse (pure "\n\n\n")
        -- >>> Array.concat
        -- >>> String.joinWith "\n"
    where
        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Doc Unit
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            D.text "let T = ./Types.dhall"
            <> D.break <> D.break <> D.text "in"
            <> D.break <> D.indent (D.text "T.collapseAt")
            <> brindent2
                [ D.text "{ id = " <> (quote $ unwrap subjectRec.id)
                , D.text ", name = " <> quote subjectRec.name
                , D.text ", platform = " <> D.text "T.Platform.<TODO>"
                , D.text ", playtime = " <> D.text "T.Playtime.<TODO>"
                , D.text "}"
                , (D.enclose (D.text "(") (D.text ")") $ mbdaterec subjectRec.trackedAt) <> D.space <> D.text "("
                ]
            <> fold (mapWithIndex convertGroup groups)

            -- <> (Array.concat $ Array.intersperse (pure "\n\n") $ convertGroup <$> groups)
            -- <> [ indent <> ")" ]

        convertGroup :: Int -> { group :: Group, items :: Array ItemRec } -> Doc Unit
        convertGroup index { group, items } =
            let groupRec = unwrap group in
            D.break <> D.break <> D.indent
                (  (if index == 0 then mempty else D.text "#" <> D.space)
                <> D.text "T.group" <> D.space <> quote groupRec.title
                )
            -- [ indent <> "( T.group " <> quote groupRec.title <> " " <> ilarray (quote <$> unwrap <$> unwrap groupRec.path)
            -- ]
            -- <> pure (array (indent <> indent) (convertItem <$> items))
            -- <> [ indent <> ")"
            --    ]

        {-
        convertGroup :: { group :: Group, items :: Array ItemRec } -> Array String
        convertGroup { group, items } =
            let groupRec = unwrap group in
            [ "# T.groupStats " <> quote groupRec.title <> " " <> ilarray (quote <$> unwrap <$> unwrap groupRec.path)
            ]
            <> pure (array (indent <> indent) (convertItem <$> items))

        convertItem :: ItemRec -> String
        convertItem itemRec =
            "T.kv_ " <> quote itemRec.name <> " " <> (alignModifiers $ convertModifier <$> itemRec.modifiers)

        convertModifier :: ModifierRec -> Array String
        convertModifier modRec =
            case modRec.mkind of
                P ->
                    let (mbPKey :: Maybe P.Key) = decodeKey modRec.mkey in
                    case mbPKey of
                        Just P.KRating ->
                            pure "p_rating " -- <> quote (modRec.value)
                        Just P.KPriority ->
                            pure "p_priority " -- <> quote (modRec.value)
                        Just P.KTask ->
                            pure "p_task " -- <> quote (modRec.value)
                        Nothing ->
                            pure $ quote modRec.mkey
                S ->
                    let (mbSKey :: Maybe S.Key) = decodeKey modRec.mkey in
                    case mbSKey of
                        Just (S.KProgress _) ->
                            withImpl @Progress _progressToDhall modRec.value
                        Just S.KEarnedAt ->
                            withImpl @CT.SDate (sdaterec >>> prefix "// T.inj/date" >>> pure) modRec.value
                        Just S.KDescription ->
                            withImpl @String (quote >>> prefix "// T.inj/det" >>> pure) modRec.value
                        Just S.KReference ->
                            withImpl @GroupPath
                                (unwrap >>> map (unwrap >>> quote) >>> ilarray >>> prefix "// T.inj/self" >>> pure)
                                modRec.value
                        Just S.KTags ->
                            withImpl @(Tags item_tag)
                                (unwrap >>> map (tagContent >>> quote) >>> ilarray >>> prefix "// T.inj/tags" >>> pure)
                                modRec.value
                        Nothing ->
                            pure $ quote modRec.mkey

        alignModifiers :: Array (Array String) -> String
        alignModifiers =
            map (\arr ->
                    case Array.uncons arr of
                        Just { head, tail } -> head <> String.joinWith ("\n" <> indent3) tail
                        Nothing -> ""
                )
            >>> String.joinWith " "
        -}


withImpl :: forall @t. ReadForeign t => (t -> Array String) -> Foreign -> Array String
withImpl f frgn = (readImpl frgn :: F t) # runExcept # either (const $ pure "{- <error> -}") f


brindent2 :: forall a. Array (Doc a) -> Doc a
brindent2 = map (\d -> D.break <> D.indent (D.indent d)) >>> fold


_progressToDhall :: Progress -> Array (Doc Unit)
_progressToDhall = case _ of
    None ->        pure $ D.text "T.v_empty"
    Unknown ->     pure $ D.text "T.v_empty"
    PInt i ->      pure $ wrapbrkD $ D.text "T.v_i" <+> (prefixnosp "+" $ show i)
    PNumber n ->   pure $ wrapbrkD $ D.text "T.v_n" <+> D.text (show n)
    PText text ->  pure $ wrapbrkD $ D.text "T.v_t" <+> quote text
    ToComplete { done } -> pure $ D.text $ if done then "T.v_done" else "T.v_none"
    PercentI i ->  pure $ wrapbrkD $ D.text "T.v_pcti" <+> (prefixnosp "+" $ show i)
    PercentN n ->  pure $ wrapbrkD $ D.text "T.v_pct" <+> D.text (show n)
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ wrapbrkD $ D.text "T.v_pctx" <+> D.text sign_s <+> D.text (show pct)
    ToGetI { got, total } ->
        pure $ wrapbrkD $ D.text "T.v_pi " <> wrecord
            [ "got" /\ ("+" <> show got)
            , "total" /\ ("+" <> show total)
            ]
    ToGetN { got, total } ->
        pure $ wrapbrkD $ D.text "T.v_pd " <> wrecord
            [ "got" /\ show got
            , "total" /\ show total
            ]
    OnTime timeRec ->
        pure $ wrapbrkD $ D.text "T.v_time " <> wrecord
            [ "hrs" /\ ("+" <> show timeRec.hrs)
            , "min" /\ ("+" <> show timeRec.min)
            , "sec" /\ ("+" <> show timeRec.sec)
            ]
    OnDate sdate ->
        pure $ wrapbrkD $ D.text "T.v_date " <> sdaterec sdate
        {-
        let dateRec = CT.dateToRec sdate
        in pure $ "T.v_date " <> wrecord
            [ "day"  /\ ("+" <> show dateRec.day)
            , "mon"  /\ ("+" <> show dateRec.mon)
            , "year" /\ ("+" <> show dateRec.year)
            ]
        -}
    PerI { amount, per } ->
        pure $ wrapbrkD $ D.text "T.v_per" <+> (prefixnosp "+" $ show amount) <+> quote per
    PerN { amount, per } ->
        pure $ wrapbrkD $ D.text "T.v_per" <+> D.text (show amount) <+> quote per
    MeasuredI { amount, measure } ->
        pure $ wrapbrkD $ D.text "T.v_mes" <+> (prefixnosp "+" $ show amount) <+> quote measure
    MeasuredN { amount, measure } ->
        pure $ wrapbrkD $ D.text "T.v_mesd" <+> D.text (show amount) <+> quote measure
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ wrapbrkD $ D.text "T.v_mesx" <+> D.text sign_s <+> (prefixnosp "+" $ show amount) <+> quote measure
    RangeI { from, to } ->
        pure $ wrapbrkD $ D.text "T.v_rng " <> wrecord
            [ "from" /\ ("+" <> show from)
            , "to" /\ ("+" <> show to)
            ]
    RangeN { from, to } ->
        pure $ wrapbrkD $ D.text "T.v_rngd " <> wrecord
            [ "from" /\ show from
            , "to" /\ show to
            ]
    Task taskP ->
        pure $ wrapbrkD $ D.text "T.v_proc " <> quoteD (vtaskP taskP)
    LevelsI { reached, levels } ->
        let
            levelrec lrec =
                wrecordD
                    [ "maximum" /\ (prefixnosp "+" $ show lrec.maximum)
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
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
                wrecordD
                    [ "maximum" /\ D.text (show lrec.maximum)
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
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
                wrecordD
                    [ "gives" /\ quote lrec.gives
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ D.text "(" <+> D.text "T.v_lvls"
        , D.indent $ sfield $ "reached" /\ ("+" <> show reached)
        , D.indent $ xfield "levels"
        ]
        <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
        [ D.indent recend
        , D.text ")"
        ]
    LevelsE levelsE ->
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
                wrecordD
                    [ "name" /\ quote lrec.name
                    , "proc" /\ pvptaskP lrec.proc
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ D.text "(" <+> D.text "T.v_lvlp"
        , D.indent $ xfield "levels"
        ]
        <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
        [ D.indent recend
        , D.text ")"
        ]
    LevelsC levelsC ->
        pure $ wrapbrkD $ D.text "T.v_lvlc" <+>
            wrecordD
              [ "reached" /\ (prefixnosp "+" $ show levelsC.levelReached)
              , "current" /\ (prefixnosp "+" $ show levelsC.reachedAtCurrent)
              , "total" /\ (prefixnosp "+" $ show levelsC.totalLevels)
              , "maxcurrent" /\ (prefixnosp "+" $ show levelsC.maximumAtCurrent)
              , "date" /\ mbdaterec levelsC.date
              ]
    LevelsO { reached, levels } ->
        let
            levelrec lrec =
                wrecordD
                    [ "maximum" /\ mbgenD (D.text "Integer") (\v -> prefixnosp "+" $ show v) lrec.mbMaximum
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ D.text "(" <+> D.text "T.v_lvli"
        , D.indent $ sfield $ "reached" /\ ("+" <> show reached)
        , D.indent $ xfield "levels"
        ]
        <> ((D.indent <<< D.indent) <$> arrayD (levelrec <$> levels)) <>
        [ D.indent recend
        , D.text ")"
        ]
    Error err ->
        [ D.text "ERR" ] -- FIXME:
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
ilarrayD = wraparrD <<< joinWith (D.text ",")


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
prefix p s = D.text p <+> D.text s


prefixnosp :: forall a. String -> String -> Doc a
prefixnosp p s = D.text p <> D.text s


field :: forall a. (String /\ String) -> Doc a
field = Tuple.uncurry $ \n v -> D.text n <+> D.text "=" <+> D.text v


fieldD :: forall a. (String /\ Doc a) -> Doc a
fieldD = Tuple.uncurry $ \n docv -> D.text n <+> D.text "=" <+> docv


joinWith :: forall a. Doc a -> Array (Doc a) -> Doc a
joinWith sep =
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


daterecf :: forall a. CT.SDateRec -> Doc a
daterecf dateRec =
    wrecord
        [ "day"  /\ ("+" <> show dateRec.day)
        , "mon"  /\ ("+" <> show dateRec.mon)
        , "year" /\ ("+" <> show dateRec.year)
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
