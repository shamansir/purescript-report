module GameLog.Types.ManyGamesStats where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Array (findMap) as Array
import Data.Tuple.Nested ((/\), type (/\))

import Report (Report, class ToReport)
import Report (buildG) as Report
import Report.Class (convertFrom)
import Report.Group (Group)
import Report.Convert.Generic (class ToExport, class ToImport, uniqueIdFromTitle)
import Report.Chain as C
import Report.Web.Component as ForWeb
import Report.Impl.Item (Item(..))
import Report.Impl.Subject (Subject(..), SubjectId)
import Report.Tabular as Tabular
import Report.Decorator as Dec
import Report.Decorators.Progress (Progress(..), Relation(..)) as Progress
import Report.Decorators.Tabular.TabularValue as TV
import Report.Decorators.Tags (RawTag)

import GameLog.Types (Platform(..))
import GameLog.Types.Game (Game(..), GameId(..), GameTag(..), loadPlatformFromTag, loadSourceFromTag)
import GameLog.Types.Achievement (Achievement(..), Tag(..), TagKind)
import GameLog.Types.SingleGameStats (GameAchievements)

newtype RawAchievements = RawAchievements (Map Game GameAchievements)

derive instance Newtype RawAchievements _

type GamesReport = Report Game Group Achievement
newtype GamesReportW = GRW GamesReport
derive instance Newtype GamesReportW _

instance ToReport Game Group Achievement RawAchievements where
    toReport :: RawAchievements -> GamesReport
    toReport = unwrap >>> map unwrap >>> Map.toUnfoldable >>> map (map Map.toUnfoldable) >>> Report.buildG


instance ToReport Game Group Achievement GamesReportW where
    toReport :: GamesReportW -> GamesReport
    toReport = unwrap


fromArray :: Array (Game /\ GameAchievements) -> RawAchievements
fromArray = Map.fromFoldable >>> RawAchievements


instance ForWeb.Is GameId GameTag TagKind Tag Game Group Achievement RawAchievements
instance ForWeb.Has       GameTag         Tag Game Group Achievement RawAchievements
instance ForWeb.Modify                    Tag      Group Achievement RawAchievements
instance ToExport  GameId GameTag         Tag Game Group Achievement RawAchievements
instance ToImport  GameId GameTag         Tag Game Group Achievement RawAchievements where
    convertSubjectId idx name = Just <<< convertSubjectId idx name
    convertSubject    = Just <<< convertSubject
    convertSubjectTag = Just <<< convertSubjectTag
    convertGroup      = Just
    convertItem       = Just <<< convertItem
    convertItemTag    = Just <<< convertItemTag


instance ForWeb.Is GameId GameTag TagKind Tag Game Group Achievement GamesReportW
instance ForWeb.Has       GameTag         Tag Game Group Achievement GamesReportW
instance ForWeb.Modify                    Tag      Group Achievement GamesReportW
instance ToExport  GameId GameTag         Tag Game Group Achievement GamesReportW
instance ToImport  GameId GameTag         Tag Game Group Achievement GamesReportW where
    convertSubjectId idx name = Just <<< convertSubjectId idx name
    convertSubject    = Just <<< convertSubject
    convertSubjectTag = Just <<< convertSubjectTag
    convertGroup      = Just
    convertItem       = Just <<< convertItem
    convertItemTag    = Just <<< convertItemTag


convertSubject :: Subject GameId GameTag -> Game
convertSubject (Subject subjectRec) =
    Game
    { name        : subjectRec.name
    , gameId      : subjectRec.id
    , mbPlatform  : subjectRec.tags # Array.findMap loadPlatformFromTag
    , mbSource    : subjectRec.tags # Array.findMap loadSourceFromTag
    , mbTrackedAt : subjectRec.tabular # Tabular.findMapV "trackedAt" (case _ of
        TV.TVAtomic (TV.TVDate date) -> Just date
        _ -> Nothing)
    , mbPlaytime  : subjectRec.tabular # Tabular.findMapV "playtime" (case _ of
        TV.TVAtomic (TV.TVTime time) -> Just $ Progress.REqual /\ time
        TV.TVAtomic (TV.TVDecorator (Dec.SProgress (Progress.RelTime rel time))) -> Just $ rel /\ time
        _ -> Nothing)
    , stats       : subjectRec.stats
    }


convertItemTag :: RawTag -> Tag
convertItemTag = unwrap >>> _.id >>> C.fromNEArray >>> convertFrom >>> fromMaybe (Tag "?")


convertSubjectTag :: RawTag -> GameTag
convertSubjectTag = unwrap >>> _.id >>> C.fromNEArray >>> convertFrom >>> fromMaybe (PlatformTag Shared)


convertItem :: Item Tag -> Achievement
convertItem  (Item itemRec) =
    Achievement
        { locked        : false -- FIXME: may be import it?
        , mbDescription : itemRec.decorators # Dec.getDescription
        , mbEarnedAt    : itemRec.decorators # Dec.getEarnedAt
        , mbInternalId  : Nothing -- FIXME: may be import it?
        , mbReference   : itemRec.decorators # Dec.getReference
        , mbTitle       : Nothing -- Just itemRec.title
        , name          : itemRec.title
        , progress      : itemRec.decorators # Dec.firstProgress # fromMaybe Progress.Unknown
        , tags          : map unwrap <$> unwrap $ itemRec.tags
        }


convertSubjectId :: Int -> String -> Maybe SubjectId -> GameId
convertSubjectId _ name = maybe (REP $ uniqueIdFromTitle name) (unwrap >>> REP)


-- bindToAchievement :: Achievement -> Group -> Group
-- bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ Ach.getProgress ach }


