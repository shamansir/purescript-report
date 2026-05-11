module Report.Decorators.Levels where

import Prelude

import Data.Maybe (Maybe)

import Report.Core (SDateRec, STimeRec) as CT

import Report.Decorators.Task (TaskP)

type DateRec = CT.SDateRec
type TimeRec = CT.STimeRec


type LevelN = { maximum :: Number, name :: String, date :: Maybe DateRec }
type LevelI = { maximum :: Int, name :: String, date :: Maybe DateRec }
type LevelS = { gives :: String, date :: Maybe DateRec }
type LevelP = { name :: String, proc :: TaskP, date :: Maybe DateRec, endDate :: Maybe DateRec }
type LevelO = { mbMaximum :: Maybe Int, name :: String, date :: Maybe DateRec }


type LevelsN = { reached :: Number, levels :: Array LevelN } -- every level has number maximum, name and probably a date when was achieved, `reached` value is also a number and holds the scored value
type LevelsI = { reached :: Int, levels :: Array LevelI } -- every level has integer maximum, name and probably a date when was achieved, `reached` value is also an integerr and holds the scored value
type LevelsS = { reached :: Int, levels :: Array LevelS } -- every level gives something textual, `reached` value is a level index
type LevelsO = { reached :: Int, levels :: Array LevelO } -- every level has optional integer maximum (when known), name and probably a date when was achieved, reached value is also an integer
type LevelsE = { reached :: Int, total :: Int } -- exact levels are unknown, we only have a value of levels achieved and total level count
type LevelsC = -- exact levels are unknown, we only have a value of levels achieved, total level count, and a value reached at current level and maximum value at current level
    { levelReached :: Int
    , totalLevels :: Int
    , reachedAtCurrent :: Int
    , maximumAtCurrent :: Int
    , date :: Maybe DateRec
    }
type LevelsP = { levels :: Array LevelP } -- every level has task inside, name and probably a date when was achieved, reached value is not stored and intended to be calculated from the task data


