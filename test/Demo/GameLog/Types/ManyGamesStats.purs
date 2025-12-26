module GameLog.Types.ManyGamesStats where

import Prelude

import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested ((/\), type (/\))

import Report (Report, class ToReport)
import Report (fromMap) as Report
import Report.Group (Group)

import GameLog.Types.Game (Game)
import GameLog.Types.Achievement (Achievement)
import GameLog.Types.SingleGameStats (GameAchievements)

newtype RawAchievements = RawAchievements (Map Game GameAchievements)

derive instance Newtype RawAchievements _

type GamesReport = Report Game Group Achievement

instance ToReport Game Group Achievement RawAchievements where
    toReport :: RawAchievements -> Report Game Group Achievement
    toReport = unwrap >>> map unwrap >>> Report.fromMap


fromArray :: Array (Game /\ GameAchievements) -> RawAchievements
fromArray = Map.fromFoldable >>> RawAchievements


-- bindToAchievement :: Achievement -> Group -> Group
-- bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ Ach.getProgress ach }


