module GameLog.Dhall where

import Prelude

import Control.Monad.Except as ME
import Data.Array (snoc, catMaybes, concat, find) as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, hush, blush)
import Data.Foldable (foldl, foldr)
import Data.List (toUnfoldable) as List
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable, values) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (joinWith) as String
import Data.Tuple (curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Foreign (Foreign, F, fail, ForeignError(..), renderForeignError)

-- import Input.CV.Key (w)
import Report.Group (GroupPath(..), PathSegment(..))
import Report.Stats (Stats(..))
import Report.Task (TaskP(..))
import Report.Progress (DateRec, LevelsI, LevelsN, LevelsS, LevelsP, Progress(..), TimeRec, PValueTag(..), _readProgress)
import Report.Core as CT

import GameLog.Types as GLT
import GameLog.Types.Achievement (Achievement(..), collectStatsRaw, getProgress)
import GameLog.Types.Game (Game(..), GameId(..), Source(..))
import GameLog.Types.Group (Group(..), isGroupAt, setStats)
import GameLog.Types.SingleGameStats (GameAchievements(..), collectStats)

import Yoga.JSON (class ReadForeign, readImpl)


{- DHALL Import -}


type DHallAchRec =
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


newtype DhallGameId = DhallGameId String


newtype DhallGame = DhallGame { game :: Game, groups :: Map Group (Array Achievement) }
derive instance Newtype DhallGame _


newtype FromDhall = FromDhall (Array DhallGame)
derive instance Newtype FromDhall _


type DhallGameJson =
    { trackedAt :: Maybe DateRec
    , properties :: Array DHallAchRec
    , game ::
        { name :: String
        , id :: String
        , platform :: String
        }
    }


type DhallJson =
    { collection :: Array DhallGameJson
    }


instance ReadForeign FromDhall where
    readImpl :: Foreign -> F FromDhall
    readImpl frgn = do
        (dhallJSON :: DhallJson) <- readImpl frgn
        pure $ collectFromDHall dhallJSON -- $ foldl insertByRef Map.empty dhallJSON.properties


instance ReadForeign DhallGame where
    readImpl :: Foreign -> F DhallGame
    readImpl frgn = do
        (dhallGameJSON :: DhallGameJson) <- readImpl frgn
        pure $ processGame dhallGameJSON -- $ foldl insertByRef Map.empty dhallJSON.properties


platformFromDhall :: String -> Maybe GLT.Platform
platformFromDhall = case _ of
    "GPlay" -> Just GLT.Android
    "Switch" -> Just GLT.NintendoSwitch
    "Steam" -> Just GLT.Steam
    _ -> Nothing


collectFromDHall :: DhallJson -> FromDhall
collectFromDHall { collection } = -- TODO: use `trackedAt`
    FromDhall $ processGame <$> collection


processGame :: DhallGameJson -> DhallGame
processGame { properties, game } =
    DhallGame $ let
        gameAchievements = fillSelfRefs $ fillWithStats $ groupAchievements collectedAchievements
    in
        { game :
            Game
                { gameId : DHL game.id
                , name : game.name
                , mbPlatform : platformFromDhall game.platform
                , mbSource : Just S_Dhall
                , stats : collectStats $ GameAchievements gameAchievements
                }
        , groups : gameAchievements
        }
    where
        emptyGroupsMap :: Map Group (Array Achievement)
        emptyGroupsMap = Map.fromFoldable $ emptyGroupPair <$> {- fillIndexPaths indicesMap -} collectedGroups
        collectedGroups :: Array Group
        collectedGroups = Array.catMaybes $ isGroup <$> properties
        collectedAchievements :: Array (GroupPath /\ Achievement)
        collectedAchievements = Array.catMaybes $ isAchievement <$> properties
        emptyGroupPair :: Group -> Group /\ Array Achievement
        emptyGroupPair = flip (/\) []

        skipOrAdd :: GroupPath -> Achievement -> Group /\ Array Achievement -> Group /\ Array Achievement
        skipOrAdd groupPath ach (group /\ achs) = group /\ if isGroupAt groupPath group then Array.snoc achs ach else achs
        insertByRef :: Map Group (Array Achievement) -> GroupPath /\ Achievement -> Map Group (Array Achievement)
        insertByRef map (groupPath /\ achv) =
            -- FIXME: very slow since for every achievement we search for a group where to add it by unwinding / and winding back the map
            Map.fromFoldable $ skipOrAdd groupPath achv <$> (Map.toUnfoldable map :: Array (Group /\ Array Achievement))
        groupAchievements :: Array (GroupPath /\ Achievement) -> Map Group (Array Achievement)
        groupAchievements =
            foldl insertByRef emptyGroupsMap
        fillWithStats :: Map Group (Array Achievement) -> Map Group (Array Achievement)
        fillWithStats withoutStats =
            Map.fromFoldable $ collectGroupStats <$> (Map.toUnfoldable withoutStats :: Array (Group /\ Array Achievement))
        fillSelfRefs :: Map Group (Array Achievement) -> Map Group (Array Achievement)
        fillSelfRefs withoutSelfRefs =
            let allAchievements = Array.concat $ List.toUnfoldable $ Map.values withoutSelfRefs
            in Map.fromFoldable $ lmap (fillSelfRef allAchievements) <$> (Map.toUnfoldable withoutSelfRefs :: Array (Group /\ Array Achievement))
        collectGroupStats :: Group /\ Array Achievement -> Group /\ Array Achievement
        collectGroupStats (group /\ achievements) = setStats (collectStatsRaw achievements) group /\ achievements
        references :: Group -> Achievement -> Boolean
        references (Group group) (Achievement achRec) =
            case achRec.mbReference of
                Just groupRef -> groupRef == group.path
                Nothing -> false
        fillSelfRef :: Array Achievement -> Group -> Group
        fillSelfRef allAchievements (Group group) =
            case Array.find (references $ Group group) allAchievements of
                Just achievement ->
                    Group $ group { stats = SFromProgress $ getProgress achievement }
                Nothing -> Group group


dhallRecToAchievement :: DHallAchRec -> Achievement
dhallRecToAchievement rec =
    Achievement
        { name : fromMaybe "" rec.key
        , progress : case rec.value of
            Just { t, v } ->
                either
                    (NEL.toUnfoldable
                        >>> map renderForeignError
                        >>> String.joinWith "; "
                        >>> Error)
                    identity
                $ ME.runExcept $ _readProgress (PValueTag t) v
            Nothing -> None
        , mbTitle : rec.title
        , mbDescription : rec.detailed
        , mbInternalId : Nothing
        , mbEarnedAt : rec.date <#> CT.dateFromRec
        , mbReference : GroupPath <$> map PathSegment <$> rec.selfRef
        , locked : false
        , tags : fromMaybe [] rec.tags
        }


achievementToDhallRec :: Achievement -> DHallAchRec
achievementToDhallRec (Achievement ach) =
    { key : if ach.name == "" then Nothing else Just ach.name
    , title : ach.mbTitle
    , kind : Nothing
    , ref : []
    , detailed : ach.mbDescription
    , selfRef : Nothing
    , date : ach.mbEarnedAt <#> CT.dateToRec
    , tags : if ach.tags == [] then Nothing else Just ach.tags
    , value : Nothing
    }

    {-
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

readDhallRec :: DHallAchRec -> Either Group (GroupPath /\ Achievement)
readDhallRec ach =
    maybe
      (Right $ pathFromRef ach.ref /\ dhallRecToAchievement ach)
      (\title -> Left $
        Group
          { title : title
          , stats : SYetUnknown
          , path : pathFromRef ach.ref
          }
      )
      ach.title


pathFromRef :: Array String -> GroupPath
pathFromRef ref = GroupPath $ PathSegment <$> ref


isGroup :: DHallAchRec -> Maybe Group
isGroup = readDhallRec >>> blush

isAchievement :: DHallAchRec -> Maybe (GroupPath /\ Achievement)
isAchievement = readDhallRec >>> hush


dhallToAchievements :: FromDhall -> Array (Game /\ GameAchievements)
dhallToAchievements = unwrap >>> map unwrap >>> map \{ game, groups } -> game /\ GameAchievements groups


_writeDhallProgressLines :: Progress -> Array String
_writeDhallProgressLines = case _ of
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
        field :: (String /\ String) -> String
        field = Tuple.uncurry $ \n v -> n <> " = " <> v
        sfield p = "{ " <> field p
        nfield p = ", " <> field p
        xfield n = ", " <> n <> " ="
        sxfield n = "{ " <> n <> " ="
        recend = "}"
        array aindent items = aindent <> "[ " <>  String.joinWith ("\n" <> aindent <> ",") items <> "\n" <> aindent <> "]"
        wrecord :: Array (String /\ String) -> String
        wrecord fields = "{" <> String.joinWith ", " (field <$> fields) <> " }"
        daterecf dateRec =
            wrecord
                [ "day"  /\ ("+" <> show dateRec.day)
                , "mon"  /\ ("+" <> show dateRec.mon)
                , "year" /\ ("+" <> show dateRec.year)
                ]
        sdaterec sdate =
            daterecf $ CT.dateToRec sdate
        mbgen :: forall a. String -> (a -> String) -> Maybe a -> String
        mbgen t f = maybe ("None " <> t) $ \v -> "Some " <> f v
        mbdaterec =
            mbgen "T.DATE" daterecf
        vtaskP = case _ of
            TTodo -> "T.p_todo"
            TDoing -> "T.p_doing"
            TDone -> "T.p_done"
            TWait -> "T.p_wait"
            TNow -> "T.p_now"
            TCanceled -> "T.p_canceled"
            TLater -> "T.p_later"
        pvptaskP task = vtaskP task <> "_"


_writeDhallAchievementLine :: Achievement -> Array String
_writeDhallAchievementLine (Achievement ach) =
      [ "T.kv_" <> " " <> "\"" <> ach.name <> "\"" <> " "
      ] <> _writeDhallProgressLines ach.progress
      <> case ach.mbDescription of
           Just description -> pure "" -- TODO:
           Nothing -> pure ""

