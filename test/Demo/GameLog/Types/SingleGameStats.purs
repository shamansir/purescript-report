module GameLog.Types.SingleGameStats where

import Prelude

import Data.Array (length, concat) as Array
import Data.Map (Map)
import Data.Map (size, values) as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (foldl)
import Data.List (toUnfoldable) as List

import Report.Group (Group)
import Report.Decorators.Stats (Stats)

import GameLog.Types.Achievement (Achievement, collectStatsRaw)


{- Game Achievements -}


newtype GameAchievements = GameAchievements (Map Group (Array Achievement))
derive instance Newtype GameAchievements _


groupsCount :: GameAchievements -> Int
groupsCount = unwrap >>> Map.size


totalAchievements :: GameAchievements -> Int
totalAchievements = unwrap >>> Map.values >>> map Array.length >>> foldl (+) 0


collectStats :: GameAchievements -> Stats
collectStats (GameAchievements groups) =
    Map.values groups # List.toUnfoldable # Array.concat # collectStatsRaw

