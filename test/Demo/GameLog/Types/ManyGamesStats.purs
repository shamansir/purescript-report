module GameLog.Types.ManyGamesStats where

import Prelude

import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\), type (/\))

import Report (Report, class ToReport)
import Report (build) as Report
import Report.Group (Group)
import Report.Convert.Generic (class ToExport)
import Report.Web.Component as ForWeb

import GameLog.Types.Game (Game, GameId, GameTag)
import GameLog.Types.Achievement (Achievement, Tag, TagKind)
import GameLog.Types.SingleGameStats (GameAchievements)

newtype RawAchievements = RawAchievements (Map Game GameAchievements)

derive instance Newtype RawAchievements _

type GamesReport = Report Game Group Achievement

instance ToReport Game Group Achievement RawAchievements where
    toReport :: RawAchievements -> GamesReport
    toReport = unwrap >>> map unwrap >>> Map.toUnfoldable >>> map (map Map.toUnfoldable) >>> Report.build


fromArray :: Array (Game /\ GameAchievements) -> RawAchievements
fromArray = Map.fromFoldable >>> RawAchievements


instance ForWeb.Is GameId GameTag TagKind Tag Game Group Achievement RawAchievements
instance ForWeb.Has       GameTag         Tag Game Group Achievement RawAchievements
instance ForWeb.Modify                    Tag      Group Achievement RawAchievements
instance ToExport  GameId GameTag         Tag Game Group Achievement RawAchievements


-- bindToAchievement :: Achievement -> Group -> Group
-- bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ Ach.getProgress ach }


