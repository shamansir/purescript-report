module Report.Stats where

import Prelude

import Data.Int (toNumber, floor) as Int
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Array (length) as Array

import Report.Progress (Progress(..))
import Report.Task (TaskP(..))

{- Stats -}


data Stats
    = SGotTotal { got :: Int, total :: Int }
    | SWithProgress { total :: Int, got :: Int, onTheWay :: Int }
    -- | SCompletionStatus GLT.Completion
    | SFromProgress Progress
    | SNotRelevant -- means everything is just values
    | SYetUnknown


derive instance Eq Stats
instance Ord Stats where
    compare statsA statsB = compare (weightOf statsA) (weightOf statsB)


defaultStats :: Stats
defaultStats = SYetUnknown


{- GotTotal -}


data GotTotal
    = Defined { got :: Int, total :: Int }
    | GTStatsValue
    | Undefined


derive instance Eq GotTotal
derive instance Ord GotTotal


weightOf :: Stats -> Number
weightOf = case _ of
    SGotTotal { got } -> Int.toNumber got -- total can be 0
    SWithProgress { got, onTheWay } -> Int.toNumber got + (Int.toNumber onTheWay * 0.2)  -- total can be 0
    SFromProgress progress ->
        case gotTotalFrom progress of
            Defined { got } -> Int.toNumber got
            _ -> -0.5
    -- CompletionStatus GLT.Beaten -> 1.0
    -- CompletionStatus GLT.Completed -> 1.0
    -- CompletionStatus _ -> 1.0
    -- SCompletionStatus _ -> 1.0
    SNotRelevant -> -1.0
    SYetUnknown -> -2.0


gotTotalFromStats :: Stats -> GotTotal
gotTotalFromStats = case _ of
    SGotTotal { got, total } -> Defined { got, total }
    SWithProgress { got, total } -> Defined { got, total }
    SFromProgress progress -> gotTotalFrom progress
    -- SCompletionStatus _ -> Undefined
    SNotRelevant -> GTStatsValue
    SYetUnknown -> Undefined


gotTotalFrom :: Progress -> GotTotal
gotTotalFrom =
    case _ of
        None -> Undefined
        Unknown -> Undefined
        PText _ -> GTStatsValue
        PNumber _ -> GTStatsValue
        PInt _ -> GTStatsValue
        MeasuredI _ -> GTStatsValue
        MeasuredN _ -> GTStatsValue
        MeasuredSign _ -> GTStatsValue
        PerI _ -> GTStatsValue
        PerN _ -> GTStatsValue
        OnDate _ -> GTStatsValue
        OnTime _ -> GTStatsValue
        RangeI _ -> GTStatsValue
        RangeN _ -> GTStatsValue
        ToComplete { done } -> Defined { got : if done then 1 else 0, total : 1 }
        Task task -> Defined { got : if task == TDone then 1 else 0, total : 1 }
        ToGetI { got, total } -> Defined { got, total }
        ToGetN { got, total } -> Defined { got : Int.floor got, total : Int.floor total }
        PercentI i -> Defined { got : i, total : 100 }
        PercentN n -> Defined { got : Int.floor $ n * 100.0 , total : 100 }
        PercentSign { pct, sign } ->
            Defined { got : if (sign > 0) then Int.floor $ pct * 100.0 else 0, total : 100 }
        LevelsN { reached, levels } ->
            let
                maximumTotal = foldl max 0.0 $ _.maximum <$> levels
            in
                Defined { got : Int.floor reached, total : Int.floor maximumTotal }
        LevelsI { reached, levels } ->
            let
                maximumTotal = foldl max 0 $ _.maximum <$> levels
            in
                Defined { got : reached, total : maximumTotal }
        LevelsO { reached, levels } ->
            let
                maximumTotal = foldl max 0 $ fromMaybe 0 <$> _.mbMaximum <$> levels
            in
                Defined { got : reached, total : maximumTotal }
        LevelsS { reached, levels } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            Defined { got : reached, total : Array.length levels }
        LevelsE { reached, total } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            Defined { got : reached, total : total }
        LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } -> -- FIXME: can be unfair
            Defined { got : levelReached, total : totalLevels }
        LevelsP { levels } ->
            Defined
                { got : foldl (+) 0 $ (\lvl -> if lvl == TDone then 1 else 0) <$> _.proc <$> levels
                , total : Array.length levels
                }
        Error _ -> Undefined


