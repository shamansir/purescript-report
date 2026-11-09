module Report.Core.ProgressRefl where

{- all the various reflections of the progress in one module so we could later join them in a fewer number of options -}

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)

import Report.Core (SDateRec)




{- NProgress -}


data NProgress
    = Achieved
    | OnTheWay Number -- FIXME: OnTheWay 1.0 == Achieved, OnTheWay 0.0 == NotAchieved
    | NotAchieved
    | StatsValue
    | Skip


derive instance Generic NProgress _
instance EncodeJson NProgress where encodeJson = genericEncodeJson
instance DecodeJson NProgress where decodeJson = genericDecodeJson


-- data IProgress
--     = IAchieved
--     | IOnTheWay { got :: Int, total :: Int } -- FIXME: OnTheWay 1.0 == Achieved, OnTheWay 0.0 == NotAchieved
--     | INotAchieved
--     | IStatsValue
--     | ISkip


derive instance Eq NProgress


{- ReachedOrNot -}


data ReachedOrNot
    = Reached
    | ReachedAt SDateRec
    | ReachedDuring { start :: Maybe SDateRec, end :: Maybe SDateRec }
    | InProgress
    | NotReached
    | RUnknown


{- GotTotal -}


data GotTotal -- very close to `NProgress`, merge?
    = Defined { got :: Int, total :: Int }
    | JustCount { count :: Int }
    | GTStatsValue
    | Undefined


derive instance Eq GotTotal
derive instance Ord GotTotal


type NormalizedProgress = { got :: Number, doing :: Number } -- both <= 1.0


