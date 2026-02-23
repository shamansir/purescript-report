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
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Stats (Stats(..))
import Report.Decorators.Progress (DateRec, PValueTag(..), Progress(..), ProgressJson(..))
import Report.Decorators.Progress (fromJson, _readProgress, rawToProgressJson) as Progress
import Report.Tabular (Tabular)
import Report.Tabular (findV, fromArray) as Tabular
import Report.Decorators.Tabular.TabularValue as TV
import Report.Group (Group(..), isGroupAt, setStats)
import Report.GroupPath (GroupPath(..), PathSegment(..))
import Report.Suffix (Suffix(..), Key(..)) as S
import Report.Core as CT
import Report.Core.Logic as CT
import Report.Convert.Dhall.Import (DhallItemRec)

import GameLog.Types as GLT
import GameLog.Types.Achievement (Achievement(..), collectStatsRaw, getProgress)
import GameLog.Types.Game (Game(..), GameId(..), Source(..))

import GameLog.Types.SingleGameStats (GameAchievements(..), collectStats)

import Yoga.JSON (class ReadForeign, readImpl)


{- DHALL Import -}


type DhallAchRec = DhallItemRec


newtype DhallGameId = DhallGameId String


newtype DhallGame = DhallGame { game :: Game, groups :: Map Group (Array Achievement) }
derive instance Newtype DhallGame _


newtype FromDhall = FromDhall (Array DhallGame)
derive instance Newtype FromDhall _


{-
type DhallGameJson =
    { trackedAt :: Maybe DateRec
    , properties :: Array DhallAchRec
    , game ::
        { name :: String
        , id :: String
        , platform :: String
        }
    }
-}

type DhallTabular =
    Array
        { key :: String
        , value ::
            { t :: String
            , v :: Foreign
            }
        }


type DhallSubjectJson =
    { id :: String
    , name :: String
    , properties :: Array DhallAchRec
    , tags :: Array String
    , tabular :: DhallTabular
    }


type DhallJson =
    { collection :: Array DhallSubjectJson
    }


loadTabular :: DhallTabular -> Tabular Progress
loadTabular = Tabular.fromArray <<< map loadProgress
    where
        loadProgress { key, value } =
            key /\
                (Progress.rawToProgressJson value
                    # Progress.fromJson
                    # fromMaybe Unknown
                    -- # maybe (TV.TVString "---") (TV.TVSuffix <<< S.SProgress)
                    )


instance ReadForeign FromDhall where
    readImpl :: Foreign -> F FromDhall
    readImpl frgn = do
        (dhallJSON :: DhallJson) <- readImpl frgn
        pure $ collectFromDhall dhallJSON -- $ foldl insertByRef Map.empty dhallJSON.properties


instance ReadForeign DhallGame where
    readImpl :: Foreign -> F DhallGame
    readImpl frgn = do
        (dhallGameJSON :: DhallSubjectJson) <- readImpl frgn
        pure $ processGame dhallGameJSON -- $ foldl insertByRef Map.empty dhallJSON.properties


platformFromDhall :: Maybe String -> Maybe GLT.Platform
platformFromDhall = case _ of
    Just "GPlay" -> Just GLT.Android
    Just "Switch" -> Just GLT.NintendoSwitch
    Just "Steam" -> Just GLT.Steam
    Just "Playstation5" -> Just GLT.Playstation5
    Just "IOS" -> Just $ GLT.IOS Nothing
    -- FIXME: TODO
    _ -> Nothing


collectFromDhall :: DhallJson -> FromDhall
collectFromDhall { collection } = -- TODO: use `trackedAt`
    FromDhall $ processGame <$> collection


loadProgressTextValue :: Progress -> Maybe String
loadProgressTextValue = case _ of
    PText txt -> Just txt
    _ -> Nothing


findProgressInTabular :: String -> Array { key :: String, value :: { t :: String, v :: Foreign } } -> Maybe Progress
findProgressInTabular keyName = -- FIXME: move to `Report.Tabular`
    Array.find (_.key >>> (_ == keyName))
        >>> map _.value
        >>> flip bind ( Progress.rawToProgressJson >>> Progress.fromJson)


processGame :: DhallSubjectJson -> DhallGame
processGame subject =
    DhallGame $ let
        gameAchievements = fillSelfRefs $ fillWithStats $ groupAchievements collectedAchievements
    in
        { game :
            Game
                { gameId : DHL subject.id
                , name : subject.name
                , mbPlatform : platformFromDhall $ loadProgressTextValue =<< findProgressInTabular "platform" subject.tabular
                , mbSource : Just S_Dhall
                , mbTrackedAt : gameTabluar # Tabular.findV "trackedAt" >>= case _ of
                    OnDate sdate -> Just sdate
                    _ -> Nothing
                , stats : collectStats $ GameAchievements gameAchievements
                }
        , groups : gameAchievements
        }
    where
        gameTabluar = loadTabular subject.tabular

        emptyGroupsMap :: Map Group (Array Achievement)
        emptyGroupsMap = Map.fromFoldable $ emptyGroupPair <$> {- fillIndexPaths indicesMap -} collectedGroups
        collectedGroups :: Array Group
        collectedGroups = Array.catMaybes $ isGroup <$> subject.properties
        collectedAchievements :: Array (GroupPath /\ Achievement)
        collectedAchievements = Array.catMaybes $ isAchievement <$> subject.properties
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


adaptForeignProgress :: F Progress -> Progress -- -- FIXME: see Progress.rawToProgressJson value # Progress.fromJson,  also `loadTabular` above
adaptForeignProgress =
    either
        (NEL.toUnfoldable
            >>> map renderForeignError
            >>> String.joinWith "; "
            >>> Error)
        identity
        <<< ME.runExcept


dhallRecToAchievement :: DhallAchRec -> Achievement
dhallRecToAchievement rec =
    Achievement
        { name : fromMaybe "" rec.key
        , progress : case rec.value of
            Just { t, v } -> -- FIXME: see Progress.rawToProgressJson value # Progress.fromJson, Progress._decodeProgress / DecodeKeyed.toValue, also `loadTabular` above
                adaptForeignProgress $ Progress._readProgress (PValueTag t) v
            Nothing -> None
        , mbTitle : rec.title
        , mbDescription : rec.detailed
        , mbInternalId : Nothing
        , mbEarnedAt : rec.date <#> CT.dateFromRec
        , mbReference : GroupPath <$> map PathSegment <$> rec.selfRef
        , locked : false
        , tags : fromMaybe [] rec.tags
        }


achievementToDhallRec :: Achievement -> DhallAchRec
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

readDhallRec :: DhallAchRec -> Either Group (GroupPath /\ Achievement)
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


isGroup :: DhallAchRec -> Maybe Group
isGroup = readDhallRec >>> blush


isAchievement :: DhallAchRec -> Maybe (GroupPath /\ Achievement)
isAchievement = readDhallRec >>> hush


dhallToAchievements :: FromDhall -> Array (Game /\ GameAchievements)
dhallToAchievements = unwrap >>> map unwrap >>> map \{ game, groups } -> game /\ GameAchievements groups