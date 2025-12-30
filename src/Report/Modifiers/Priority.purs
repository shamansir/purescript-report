module Report.Modifiers.Priority where

import Prelude

import Data.Either (Either(..))


newtype Priority = Priority (Either String Int)


priorityChar :: Priority -> String
priorityChar (Priority (Left str)) = str
priorityChar (Priority (Right n)) = show n


priorityA = Priority (Left "A") :: Priority
priorityB = Priority (Left "B") :: Priority
priorityC = Priority (Left "C") :: Priority


priorityNum :: Int -> Priority
priorityNum = Priority <<< Right