module GameLog.Types.Achievement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.Array (length, filter, all) as Array

import Report.Core as CT
import Report.Class (class IsItem, class IsTag, TagColors)
import Report.Group (Group(..))
import Report.GroupPath (GroupPath)
import Report.Prefix.Task (TaskP(..))
import Report.Suffix.Stats (Stats(..))
import Report.Suffix.Progress (Progress(..), NProgress(..))
import Report.Suffix (Suffixes)
import Report.Suffix as Suffixes
import Report.Prefix (Prefixes)
import Report.Prefix as Prefixes

import GameLog.Types as GLT

import Yoga.JSON (class ReadForeign, class WriteForeign)


type AchievementRec =
    { name :: String
    , progress :: Progress
    , mbTitle :: Maybe String -- TODO: remove, title is used only for groups
    , mbDescription :: Maybe String
    , mbInternalId :: Maybe String
    , mbEarnedAt :: Maybe CT.SDate
    , locked :: Boolean
    , mbReference :: Maybe GroupPath
    , tags :: Array String
    }


{- Achievement -}


newtype Achievement = Achievement AchievementRec

derive instance Newtype Achievement _
derive instance Eq Achievement
instance Show Achievement where
    show (Achievement ach) =
        ach.name
            <> case ach.mbTitle of
                Just title -> " : " <> title <> " "
                Nothing -> " "
            <> case ach.mbDescription of
                Just descr -> " : " <> descr <> " "
                Nothing -> " "
            <> show ach.progress
            <> case ach.mbEarnedAt of
                Just dateEarned -> " (" <> show dateEarned <> ")"
                Nothing -> ""


derive newtype instance ReadForeign Achievement
derive newtype instance WriteForeign Achievement


with :: (AchievementRec -> AchievementRec) -> Achievement -> Achievement
with fn = unwrap >>> fn >>> wrap


describe :: String -> Achievement -> Achievement
describe descr = with $ _ { mbDescription = Just descr }


achievement :: String -> Progress -> Achievement
achievement name progress =
    Achievement
        { name
        , progress
        , mbTitle : Nothing
        , mbDescription : Nothing
        , mbInternalId : Nothing
        , mbEarnedAt : Nothing
        , mbReference : Nothing
        , locked : false
        , tags : []
        }


{- Helpers -}


ach_ :: String -> Progress -> Achievement
ach_ = achievement


a_got :: String -> Achievement
a_got name = ach_ name (ToComplete { done : true })


a_got' :: String -> String -> Achievement
a_got' name descr = a_got name # describe descr


a_todo :: String -> Achievement
a_todo name = ach_ name (ToComplete { done : false })


a_todo' :: String -> String -> Achievement
a_todo' name descr = a_todo name # describe descr


a_prc :: String -> Int -> Achievement
a_prc name prc = ach_ name $ PercentI prc


a_prc' :: String -> String -> Int -> Achievement
a_prc' name descr = a_prc name >>> describe descr


a_itm :: String -> Int -> Int -> Achievement
a_itm name got total = ach_ name $ ToGetI { got, total }


a_itm' :: String -> String -> Int -> Int -> Achievement
a_itm' name descr got total = a_itm name got total # describe descr


{- Progress -}


getProgress :: Achievement -> Progress
getProgress = unwrap >>> _.progress


loadNProgress :: Progress -> NProgress
loadNProgress =
    case _ of
        None -> NotAchieved
        Unknown -> NotAchieved
        PText _ -> StatsValue
        PNumber _ -> StatsValue
        PInt _ -> StatsValue
        MeasuredI _ -> StatsValue
        MeasuredN _ -> StatsValue
        MeasuredSign _ -> StatsValue
        PerI _ -> StatsValue
        PerN _ -> StatsValue
        OnDate _ -> StatsValue
        OnTime _ -> StatsValue
        RangeI _ -> StatsValue
        RangeN _ -> StatsValue
        ToComplete { done } -> if done then Achieved else NotAchieved
        ToGetI { got, total } -> if got >= total && total /=   0 then Achieved else if got == 0   || total == 0   then NotAchieved else OnTheWay $ Int.toNumber $ got / total
        ToGetN { got, total } -> if got >= total && total /= 0.0 then Achieved else if got == 0.0 || total == 0.0 then NotAchieved else OnTheWay $ got / total
        PercentI i -> if i >= 100 then Achieved else if i == 0   then NotAchieved else OnTheWay $ Int.toNumber i / 100.0
        PercentN n -> if n >= 1.0 then Achieved else if n == 0.0 then NotAchieved else OnTheWay n
        PercentSign { pct, sign } ->
            if (sign > 0) && (pct >= 1.0) then Achieved
            else if (sign < 0) || (pct <= 0.0) then NotAchieved
            else OnTheWay $ if sign >= 0 then pct else 0.0
        Task task -> if task == TDone then Achieved else if task == TDoing then OnTheWay 0.5 else NotAchieved
        LevelsN { reached, levels } ->
            let
                maximumTotal = foldl max 0.0 $ _.maximum <$> levels
            in
                if reached >= maximumTotal then Achieved
                else if reached == 0.0 then NotAchieved
                else OnTheWay $ reached / maximumTotal
        LevelsI { reached, levels } ->
            let
                maximumTotal = foldl max 0 $ _.maximum <$> levels
            in
                if reached >= maximumTotal then Achieved
                else if reached == 0 then NotAchieved
                else OnTheWay $ Int.toNumber reached / Int.toNumber maximumTotal
        LevelsO { reached, levels } ->
            Skip -- Some values for levels are Maybe so we don't know for sure if user achieved something
        LevelsS { reached, levels } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            if reached + 1 >= Array.length levels then Achieved else if reached == 0 then NotAchieved else OnTheWay $ Int.toNumber reached / Int.toNumber (Array.length levels)
        LevelsE { reached, total } -> -- FIXME: reached `0` is both reached first level and reached nothing?
            if reached >= total then Achieved else if reached == 0 then NotAchieved else OnTheWay $ Int.toNumber reached / Int.toNumber total
        LevelsC { levelReached, totalLevels, reachedAtCurrent, maximumAtCurrent } -> -- FIXME: can be unfair
            if (levelReached == totalLevels) && (reachedAtCurrent == maximumAtCurrent) then Achieved
            else if (levelReached == 0) && (totalLevels /= 0) && (reachedAtCurrent == 0) then NotAchieved
            else OnTheWay $ Int.toNumber levelReached / Int.toNumber totalLevels -- FIXME: we don't add `reachedAtCurrent`
        LevelsP { levels } ->
            if Array.all (_ == TDone) $ _.proc <$> levels
              then Achieved
              else
                let reached = foldl (+) 0 $ (\lvl -> if lvl == TDone then 1 else 0) <$> _.proc <$> levels
                in if reached > 0 && Array.length levels > 0 then
                      OnTheWay $ Int.toNumber reached / Int.toNumber (Array.length levels)
                   else NotAchieved
        Error _ -> Skip


nProgressToNumber :: NProgress -> Number
nProgressToNumber = case _ of
    Achieved -> 1.0
    OnTheWay n -> min 1.0 n
    NotAchieved -> 0.0
    StatsValue -> -1.0
    Skip -> -2.0



collectStatsRaw :: Array Achievement -> Stats
collectStatsRaw flattened =
    let
        allProgressN = loadNProgress <$> getProgress <$> flattened
        nonValue = Array.filter (\i -> i /= Skip && i /= StatsValue) allProgressN
        total = Array.length nonValue
        got = Array.length $ Array.filter (_ == Achieved) nonValue
        onTheWay =
            Array.length
                $ Array.filter
                    (case _ of
                        OnTheWay _ -> true
                        _ -> false
                    )
                $ allProgressN
    in
        if (total == 0) then SNotRelevant
        else
            SWithProgress
                { total
                , got
                , onTheWay
                }


newtype Tag = Tag String


derive instance Newtype Tag _
derive newtype instance Eq Tag


instance IsTag Tag where
    tagContent :: Tag -> String
    tagContent = unwrap
    tagColors :: Tag -> TagColors
    tagColors = const $
            { background : "transparent"
            , text : "black"
            , border : "blue"
            }
    allTags :: Array Tag
    allTags = []


loadSuffixes :: Achievement -> Suffixes
loadSuffixes = unwrap >>> \achRec ->
    Suffixes.empty
    # (Suffixes.put $ Suffixes.SProgress achRec.progress)
    # (case achRec.mbReference of
        Just groupPath -> Suffixes.put $ Suffixes.SReference groupPath
        Nothing -> identity
       )
    # (case achRec.mbEarnedAt of
        Just dateEarned -> Suffixes.put $ Suffixes.SEarnedAt dateEarned
        Nothing -> identity
       )
    # (case achRec.mbDescription of
        Just descr -> Suffixes.put $ Suffixes.SDescription descr
        Nothing -> identity
       )


loadPrefixes :: Achievement -> Prefixes
loadPrefixes = const Prefixes.empty


instance IsItem Tag Achievement where
    i_name = unwrap >>> _.name
    i_mbTitle = unwrap >>> _.mbTitle
    i_locked =  unwrap >>> _.locked
    i_tags = unwrap >>> _.tags >>> map Tag
    i_suffixes = loadSuffixes
    i_prefixes = loadPrefixes


bindToAchievement :: Achievement -> Group -> Group
bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ getProgress ach }