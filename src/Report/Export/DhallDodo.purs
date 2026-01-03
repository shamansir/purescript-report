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
            <> D.break <> D.indent (D.indent $ D.text "{ id = " <> D.text (quote $ unwrap subjectRec.id))
            <> D.break <> D.indent (D.indent $ D.text ", name = " <> D.text (quote subjectRec.name))
            <> D.break <> D.indent (D.indent $ D.text ", platform = " <> D.text "T.Platform.<TODO>")
            <> D.break <> D.indent (D.indent $ D.text ", playtime = " <> D.text "T.Playtime.<TODO>")
            <> D.break <> D.indent (D.indent $ D.text "}")
            <> D.break <> D.indent (D.indent (D.enclose (D.text "(") (D.text ")") $ D.text $ mbdaterec subjectRec.trackedAt) <> D.space <> D.text "(")

            {-

            [ "let T = ./Types.dhall\n\nin\n" <> indent <> "T.collapseAt"
            , indent <> "{ id = " <> quote (unwrap subjectRec.id)
            , indent <> ", name = " <> quote subjectRec.name
            , indent <> ", platform = T.Platform.<TODO>"
            , indent <> ", playtime = T.Playtime.<TODO>"
            , indent <> "}"
            , indent <> mbdaterec subjectRec.trackedAt
            , ""
            , indent <> "("
            ]
            -}

            -- <> (Array.concat $ Array.intersperse (pure "\n\n") $ convertGroup <$> groups)
            -- <> [ indent <> ")" ]

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


_progressToDhall :: Progress -> Array String
_progressToDhall = case _ of
    None ->        pure "T.v_empty"
    Unknown ->     pure "T.v_empty"
    PInt i ->      pure $ wrapbrk $ "T.v_i" <> " " <> "+" <> show i
    PNumber n ->   pure $ wrapbrk $ "T.v_n" <> " " <> show n
    PText text ->  pure $ wrapbrk $ "T.v_t" <> " " <> quote text
    ToComplete { done } -> pure $ if done then "T.v_done" else "T.v_none"
    PercentI i ->  pure $ wrapbrk $ "T.v_pcti" <> " " <> "+" <> show i
    PercentN n ->  pure $ wrapbrk $ "T.v_pct" <> " " <> show n
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ wrapbrk $ "T.v_pctx" <> " " <> sign_s <> " " <> show pct
    ToGetI { got, total } ->
        pure $ wrapbrk $ "T.v_pi " <> wrecord
            [ "got" /\ ("+" <> show got)
            , "total" /\ ("+" <> show total)
            ]
    ToGetN { got, total } ->
        pure $ wrapbrk $ "T.v_pd " <> wrecord
            [ "got" /\ show got
            , "total" /\ show total
            ]
    OnTime timeRec ->
        pure $ wrapbrk $ "T.v_time " <> wrecord
            [ "hrs" /\ ("+" <> show timeRec.hrs)
            , "min" /\ ("+" <> show timeRec.min)
            , "sec" /\ ("+" <> show timeRec.sec)
            ]
    OnDate sdate ->
        pure $ wrapbrk $ "T.v_date " <> sdaterec sdate
        {-
        let dateRec = CT.dateToRec sdate
        in pure $ "T.v_date " <> wrecord
            [ "day"  /\ ("+" <> show dateRec.day)
            , "mon"  /\ ("+" <> show dateRec.mon)
            , "year" /\ ("+" <> show dateRec.year)
            ]
        -}
    PerI { amount, per } ->
        pure $ wrapbrk $ "T.v_per " <> " +" <> show amount <> " " <> quote per
    PerN { amount, per } ->
        pure $ wrapbrk $ "T.v_per " <> show amount <> " " <> quote per
    MeasuredI { amount, measure } ->
        pure $ wrapbrk $ "T.v_mes " <> " +" <> show amount <> " " <> quote measure
    MeasuredN { amount, measure } ->
        pure $ wrapbrk $ "T.v_mesd " <> show amount <> " " <> quote measure
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ wrapbrk $ "T.v_mesx " <> sign_s <> " +" <> show amount <> " " <> quote measure
    RangeI { from, to } ->
        pure $ wrapbrk $ "T.v_rng " <> wrecord
            [ "from" /\ ("+" <> show from)
            , "to" /\ ("+" <> show to)
            ]
    RangeN { from, to } ->
        pure $ wrapbrk $ "T.v_rngd " <> wrecord
            [ "from" /\ show from
            , "to" /\ show to
            ]
    Task taskP ->
        pure $ wrapbrk $ "T.v_proc " <> quote (vtaskP taskP)
    LevelsI { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "maximum" /\ ("+" <> show lrec.maximum)
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "( T.v_lvli"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        , ")"
        ]
    LevelsN { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "maximum" /\ show lrec.maximum
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "( T.v_lvld"
        , indent <> (sfield $ "reached" /\  show reached)
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        , ")"
        ]
    LevelsS { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "gives" /\ quote lrec.gives
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "( T.v_lvls"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        , ")"
        ]
    LevelsE levelsE ->
        [ "( T.v_lvlio"
        , wrecord
            [ "reached" /\ ("+" <> show levelsE.reached)
            , "total" /\ ("+" <> show levelsE.total)
            ]
        , ")"
        ]
    LevelsP { levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "name" /\ quote lrec.name
                    , "proc" /\ pvptaskP lrec.proc
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "( T.v_lvlp"
        , indent <> (sxfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        , ")"
        ]
    LevelsC levelsC ->
        pure $ wrapbrk $ "T.v_lvlc " <>
            wrecord
              [ "reached" /\ ("+" <> show levelsC.levelReached)
              , "current" /\ ("+" <> show levelsC.reachedAtCurrent)
              , "total" /\ ("+" <> show levelsC.totalLevels)
              , "maxcurrent" /\ ("+" <> show levelsC.maximumAtCurrent)
              , "date" /\ mbdaterec levelsC.date
              ]
    LevelsO { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "maximum" /\ mbgen "Integer" (\v -> "+" <> show v) lrec.mbMaximum
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "( T.v_lvli"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        , ")"
        ]
    Error err ->
        pure "" -- FIXME:
    where
        sfield p = "{ " <> field p
        nfield p = ", " <> field p
        xfield n = ", " <> n <> " ="
        sxfield n = "{ " <> n <> " ="
        wrapbrk p = "(" <> p <> ")"
        recend = "}"
        vtaskP = case _ of
            TTodo -> "T.p_todo"
            TDoing -> "T.p_doing"
            TDone -> "T.p_done"
            TWait -> "T.p_wait"
            TNow -> "T.p_now"
            TCanceled -> "T.p_canceled"
            TLater -> "T.p_later"
        pvptaskP task = vtaskP task <> "_"

ilarray :: Array String -> String
ilarray items = "[ " <>  String.joinWith "," items <> " ]"

array :: String -> Array String -> String
-- array aindent items = "b" <> aindent <> "[ " <>  String.joinWith ("\nx" <> aindent <> ", ") items <> "\nz" <> aindent <> "]"
array aindent items = aindent <> "[ " <>  String.joinWith ("\n" <> aindent <> ", ") items <> "\n" <> aindent <> "]"

quote :: String -> String
quote s = "\"" <> s <> "\""

prefix :: String -> String -> String
prefix p s = p <> " " <> s

field :: (String /\ String) -> String
field = Tuple.uncurry $ \n v -> n <> " = " <> v

wrecord :: Array (String /\ String) -> String
wrecord fields = "{" <> String.joinWith ", " (field <$> fields) <> " }"

daterecf :: CT.SDateRec -> String
daterecf dateRec =
    wrecord
        [ "day"  /\ ("+" <> show dateRec.day)
        , "mon"  /\ ("+" <> show dateRec.mon)
        , "year" /\ ("+" <> show dateRec.year)
        ]

sdaterec :: CT.SDate -> String
sdaterec =
    daterecf <<< CT.dateToRec

mbdaterec :: Maybe CT.SDateRec -> String
mbdaterec =
    mbgen "T.DATE" daterecf

mbgen :: forall a. String -> (a -> String) -> Maybe a -> String
mbgen t f = maybe ("None " <> t) $ \v -> "Some " <> f v


indent = "    " :: String
indent2 = indent <> indent :: String
indent3 = indent2 <> indent :: String
indent4 = indent3 <> indent :: String
