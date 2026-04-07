module Report.Decorators.Stats where

import Prelude

import Foreign (Foreign, F)

import Data.Int (toNumber, floor) as Int
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (length) as Array

import Yoga.JSON (class ReadForeign, readImpl, class WriteForeign, writeImpl)

import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Progress (Progress(..), NProgress(..))

{- Stats -}


data Stats
    = SGotTotal { got :: Int, total :: Int }
    | SWithProgress { total :: Int, got :: Int, onTheWay :: Int } (Maybe (Array NProgress)) -- Array GotTotal?
    -- | SCompletionStatus GLT.Completion
    | SFromProgress Progress
    | SCount { count :: Int }
    | SNotRelevant -- means everything is just values
    | SYetUnknown


derive instance Eq Stats
instance Ord Stats where
    compare statsA statsB = compare (weightOf statsA) (weightOf statsB)


defaultStats :: Stats
defaultStats = SYetUnknown


{- GotTotal -}


data GotTotal -- very close to `NProgress`, merge?
    = Defined { got :: Int, total :: Int }
    | JustCount { count :: Int }
    | GTStatsValue
    | Undefined


derive instance Eq GotTotal
derive instance Ord GotTotal


weightOf :: Stats -> Number
weightOf = case _ of
    SGotTotal { got } -> Int.toNumber got -- total can be 0
    SCount { count } -> Int.toNumber count
    SWithProgress { got, onTheWay } _ -> Int.toNumber got + (Int.toNumber onTheWay * 0.2)  -- total can be 0
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
    SWithProgress { got, total } _ -> Defined { got, total }
    SFromProgress progress -> gotTotalFrom progress
    -- SCompletionStatus _ -> Undefined
    SCount { count } -> JustCount { count } -- Defined { got : count, total : count }
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
        RelTime _ _ -> GTStatsValue
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


instance ReadForeign Stats where
    readImpl frgn = do
        ({ kind, stats } :: { kind :: String, stats :: Foreign }) <- readImpl frgn
        case kind of
            "GotTotal" -> do
                ( readImpl stats :: F { got :: Int, total :: Int } ) <#> SGotTotal
            "WithProgress" -> do
                ( readImpl stats :: F { got :: Int, total :: Int, onTheWay :: Int } ) <#> (\x -> SWithProgress x Nothing)
            "FromProgress" -> do
                ( readImpl stats :: F Progress ) <#> SFromProgress
            "Count" -> do
                ( readImpl stats :: F { count :: Int } ) <#> SCount
            "NotRelevant" ->
                pure SNotRelevant
            "YetUnknown" ->
                pure SYetUnknown
            _ ->
                pure SYetUnknown


instance WriteForeign Stats where
    writeImpl = case _ of
        SGotTotal { got, total } ->
            writeImpl { kind: "GotTotal", stats : writeImpl { got: got, total: total } }
        SWithProgress { got, total, onTheWay } _ ->
            writeImpl { kind: "WithProgress", stats : writeImpl { got: got, total: total, onTheWay: onTheWay } }
        SFromProgress progress ->
            writeImpl { kind: "FromProgress", stats : writeImpl progress }
        SCount { count } ->
            writeImpl { type: "Count", stats : writeImpl { count : count } }
        SNotRelevant ->
            writeImpl { type: "NotRelevant", stats : writeImpl "" }
        SYetUnknown ->
            writeImpl { kind: "YetUnknown", stats : writeImpl "" }