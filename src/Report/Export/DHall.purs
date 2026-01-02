module Report.Export.Dhall where

import Prelude

import Foreign (Foreign)

import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (uncurry) as Tuple
import Data.String (joinWith) as String
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Array as Array

import Yoga.JSON (class WriteForeign, writePrettyJSON)

import Report (Report)
import Report.Core as CT
import Report.Group (Group)
import Report.Class (class IsGroup, class IsItem, class IsSubject, class IsTag)
import Report.Modifiers.Class.ValueModify (class EncodableKey)
import Report.Export.Types
import Report.Export.Generic (toExport) as Report

import Report.Modifiers.Progress (Progress(..))
import Report.Modifiers.Task (TaskP(..))

{- type DhallItemRec =
    { key :: Maybe String
    , title :: Maybe String
    , kind :: Maybe String
    , ref :: Array String
    , detailed :: Maybe String
    , selfRef :: Maybe (Array String)
    , date :: Maybe DateRec
    , tags :: Maybe (Array String)
    , value ::
        Maybe
            { t :: String
            , v :: Foreign
            }
    }
-}


toDhall
    :: forall @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => EncodableKey subj_id
    => WriteForeign item_tag
    => IsTag subj_tag
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
        >>> Array.concat
        >>> String.joinWith "\n\n\n"
    where
        indent = "    "
        convertSubject :: { subject :: Subject, groups :: Array { group :: Group, items :: Array ItemRec } } -> Array String
        convertSubject { subject, groups } =
            let subjectRec = unwrap subject in
            [ "T.collapseAt"
            , indent <> "{ id = " <> unwrap subjectRec.id
            , indent <> ", name = " <> subjectRec.name
            , indent <> ", platform = T.Platform.<TODO>"
            , indent <> ", playtime = T.Playtime.<TODO>"
            , indent <> "}"
            , indent <> mbdaterec subjectRec.trackedAt
            , ""
            , indent <> "("
            ]
            <> (Array.concat $ Array.intersperse (pure $ "\n\n" <> indent <> "# ") $ convertGroup <$> groups)
            <> [ indent <> ")" ]

        convertGroup :: { group :: Group, items :: Array ItemRec } -> Array String
        convertGroup { group, items } =
            []
            -- [ "T.groupStats " <> quote group.title <> " " <> array (indent <> indent) (group.path) ]
            -- <> [ "" ]
            -- <> [ indent <> "[" ]
            -- <> (Array.concat $ Array.intersperse (pure $ ",") $ map (indent <> indent <> indent <>) $ map convertItem items)
            -- <> [ indent <> "]" ]



{-
    T.collapseAt
        { id = "animal-crossing"
        , name = "Animal Crossing"
        , platform = T.Platform.Other
        , playtime = T.Playtime.MoreThan { hrs = +110, min = +0, sec = +0 }
        }
        { day = +29, mon = +8, year = +2025 } (

    ( T.groupStats "Statistics" [ "00-stats" ]

        [ T.kv_ "Completion" (T.v_pcti +100)
        , T.kv_ "Chapters" (T.v_pi { done = +22, total = +22 })
        , T.kv_ "Trophies" (T.v_pi { done = +26, total = +29 }) // T.inj/self [ "00-stats", "00-trophies" ] -- 5 Silver, 10 Bronze
        , T.kv_ "Nickname" (T.v_t "elektrokilka")
        , T.kv_ "Difficulty" (T.v_t "Hurt me plenty")
        ]
    )

    # (...)
-}


_progressToDhall :: Progress -> Array String
_progressToDhall = case _ of
    None ->        pure "T.v_empty"
    Unknown ->     pure "T.v_empty"
    PInt i ->      pure $ "T.v_i" <> " " <> "+" <> show i
    PNumber n ->   pure $ "T.v_n" <> " " <> show n
    PText text ->  pure $ "T.v_t" <> " " <> quote text
    ToComplete { done } -> pure $ if done then "T.v_done" else "T.v_none"
    PercentI i ->  pure $ "T.v_pcti" <> " " <> "+" <> show i
    PercentN n ->  pure $ "T.v_pct" <> " " <> show n
    PercentSign { sign, pct } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ "T.v_pctx" <> " " <> sign_s <> " " <> show pct
    ToGetI { got, total } ->
        pure $ "T.v_pi " <> wrecord
            [ "got" /\ ("+" <> show got)
            , "total" /\ ("+" <> show total)
            ]
    ToGetN { got, total } ->
        pure $ "T.v_pd " <> wrecord
            [ "got" /\ show got
            , "total" /\ show total
            ]
    OnTime timeRec ->
        pure $ "T.v_time " <> wrecord
            [ "hrs" /\ ("+" <> show timeRec.hrs)
            , "min" /\ ("+" <> show timeRec.min)
            , "sec" /\ ("+" <> show timeRec.sec)
            ]
    OnDate sdate ->
        pure $ "T.v_date " <> sdaterec sdate
        {-
        let dateRec = CT.dateToRec sdate
        in pure $ "T.v_date " <> wrecord
            [ "day"  /\ ("+" <> show dateRec.day)
            , "mon"  /\ ("+" <> show dateRec.mon)
            , "year" /\ ("+" <> show dateRec.year)
            ]
        -}
    PerI { amount, per } ->
        pure $ "T.v_per " <> " +" <> show amount <> " " <> quote per
    PerN { amount, per } ->
        pure $ "T.v_per " <> show amount <> " " <> quote per
    MeasuredI { amount, measure } ->
        pure $ "T.v_mes " <> " +" <> show amount <> " " <> quote measure
    MeasuredN { amount, measure } ->
        pure $ "T.v_mesd " <> show amount <> " " <> quote measure
    MeasuredSign { sign, amount, measure } ->
        let sign_s = if sign > 0 then "+1" else if sign < 0 then "-1" else "+0"
        in pure $ "T.v_mesx " <> sign_s <> " +" <> show amount <> " " <> quote measure
    RangeI { from, to } ->
        pure $ "T.v_rng " <> wrecord
            [ "from" /\ ("+" <> show from)
            , "to" /\ ("+" <> show to)
            ]
    RangeN { from, to } ->
        pure $ "T.v_rngd " <> wrecord
            [ "from" /\ show from
            , "to" /\ show to
            ]
    Task taskP ->
        pure $ "T.v_proc " <> quote (vtaskP taskP)
    LevelsI { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "maximum" /\ ("+" <> show lrec.maximum)
                    , "name" /\ quote lrec.name
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "T.v_lvli"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
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
        [ "T.v_lvld"
        , indent <> (sfield $ "reached" /\  show reached)
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        ]
    LevelsS { reached, levels } ->
        let
            levelrec lrec =
                wrecord
                    [ "gives" /\ quote lrec.gives
                    , "date" /\ mbdaterec lrec.date
                    ]
        in
        [ "T.v_lvls"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        ]
    LevelsE levelsE ->
        [ "T.v_lvlio"
        , wrecord
            [ "reached" /\ ("+" <> show levelsE.reached)
            , "total" /\ ("+" <> show levelsE.total)
            ]
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
        [ "T.v_lvlp"
        , indent <> (sxfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        ]
    LevelsC levelsC ->
        pure $ "T.v_lvlc " <>
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
        [ "T.v_lvli"
        , indent <> (sfield $ "reached" /\ ("+" <> show reached))
        , indent <> (xfield "levels")
        , array (indent <> indent) $ levelrec <$> levels
        , indent <> recend
        ]
    Error err ->
        pure "" -- FIXME:
    where
        indent = "    "
        quote s = "\"" <> s <> "\""

        sfield p = "{ " <> field p
        nfield p = ", " <> field p
        xfield n = ", " <> n <> " ="
        sxfield n = "{ " <> n <> " ="
        recend = "}"
        array aindent items = aindent <> "[ " <>  String.joinWith ("\n" <> aindent <> ",") items <> "\n" <> aindent <> "]"
        vtaskP = case _ of
            TTodo -> "T.p_todo"
            TDoing -> "T.p_doing"
            TDone -> "T.p_done"
            TWait -> "T.p_wait"
            TNow -> "T.p_now"
            TCanceled -> "T.p_canceled"
            TLater -> "T.p_later"
        pvptaskP task = vtaskP task <> "_"


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