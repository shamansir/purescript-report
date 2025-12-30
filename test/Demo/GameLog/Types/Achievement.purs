module GameLog.Types.Achievement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.Array (length, filter, all) as Array
import Data.Tuple.Nested ((/\), type (/\))

import Report.Core as CT
import Report.Class (class IsItem, class IsTag, TagColors)
import Report.Group (Group(..))
import Report.GroupPath (GroupPath)
import Report.Modifiers.Task (TaskP(..))
import Report.Modifiers.Stats (Stats(..))
import Report.Modifiers.Tags (Tags(..))
import Report.Modifiers.Progress (Progress(..), NProgress(..), loadNProgress)
import Report.Modify (class ItemModify)
import Report.Suffix (Suffix, Suffixes)
import Report.Suffix as Suffixes
import Report.Prefix (Prefixes)
import Report.Prefix as Prefixes
import Report.Tabular as Tabular

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
    decodeTag :: String -> Maybe Tag
    decodeTag = Just <<< Tag
    allTags :: Array Tag
    allTags = []


loadSuffixes :: Achievement -> Suffixes Tag
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
    # (case achRec.tags of
        []  -> identity
        arr ->  Suffixes.put $ Suffixes.STags $ Tags $ Tag <$> arr
       )


applySuffixes :: Suffixes Tag -> Achievement -> Achievement
applySuffixes suffixes = with \achRec ->
    Suffixes.toArray suffixes
        # foldl applySuffix achRec
    where
        applySuffix :: AchievementRec -> (Suffixes.Key /\ Suffix Tag) -> AchievementRec
        applySuffix acc (key /\ suffix) = case suffix of
            Suffixes.SProgress prog -> acc { progress = prog }
            Suffixes.SEarnedAt sdate -> acc { mbEarnedAt = Just sdate }
            Suffixes.SDescription descr -> acc { mbDescription = Just descr }
            Suffixes.SReference gpath -> acc { mbReference = Just gpath }
            Suffixes.STags (Tags tagsArr) -> acc { tags = unwrap <$> tagsArr }


loadPrefixes :: Achievement -> Prefixes
loadPrefixes = const Prefixes.empty


instance IsItem Tag Achievement where
    i_name = unwrap >>> _.name
    i_mbTitle = unwrap >>> _.mbTitle
    i_locked =  unwrap >>> _.locked
    -- i_tags = unwrap >>> _.tags >>> map Tag
    i_suffixes = loadSuffixes
    i_prefixes = loadPrefixes
    i_tabular = const Tabular.empty


instance ItemModify Tag Achievement where
    setName newName = with \achRec -> achRec { name = newName }
    updateSuffixes newSuffixes = applySuffixes newSuffixes
    updatePrefixes _ = identity


bindToAchievement :: Achievement -> Group -> Group
bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ getProgress ach }