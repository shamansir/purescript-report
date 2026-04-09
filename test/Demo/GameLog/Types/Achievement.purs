module GameLog.Types.Achievement where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Int (toNumber) as Int
import Data.Foldable (foldl)
import Data.Array (length, filter, all, unsnoc) as Array
import Data.Tuple.Nested ((/\), type (/\))

import Report.Core as CT
import Report.Class
import Report.Chain (Chain(..))
import Report.Chain as Chain
import Report.Group (Group(..), mkGroup)
import Report.GroupPath (GroupPath, PathSegment(..))
import Report.Decorators.Task (TaskP(..))
import Report.Decorators.Stats (Stats(..))
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Progress (Progress(..), NProgress(..), loadNProgress)
import Report.Web.Decorators.Tags.Colors as TC
import Report.Modify
import Report.Decorator (Decorator, Decorators)
import Report.Decorator as Decorators
import Report.Decorator (Decorator(..)) as Dec
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
            $ Just allProgressN


newtype Tag = Tag String


derive instance Newtype Tag _
derive newtype instance Eq Tag
derive newtype instance Ord Tag
derive newtype instance ReadForeign Tag
derive newtype instance WriteForeign Tag


instance TagAlike Tag where
    tagContent :: Tag -> Chain String
    tagContent = unwrap >>> End
    tagColors :: Tag -> TagColors
    tagColors = const $ TC.tagVar10


instance ConvertTo (Chain String) Tag where
    encodeTo :: Tag -> Chain String
    encodeTo = unwrap >>> End


instance ConvertFrom (Chain String) Tag where
    decodeFrom :: Chain String -> Maybe Tag
    decodeFrom = Just <<< Tag <<< Chain.last


instance LimitedSet Tag where
    values :: Array Tag
    values = []


newtype TagKind = TagKind String
derive instance Newtype TagKind _
derive newtype instance Eq TagKind


instance ConvertTo (Chain String) TagKind where
    encodeTo :: TagKind -> Chain String
    encodeTo = unwrap >>> End


instance ConvertFrom (Chain String) TagKind where
    decodeFrom :: Chain String -> Maybe TagKind
    decodeFrom = Just <<< TagKind <<< Chain.last


instance Same TagKind where
    same :: TagKind -> TagKind -> Boolean
    same = const $ const true -- all the tags are the same kind


instance TagAlike TagKind where
    tagContent :: TagKind -> Chain String
    tagContent = unwrap >>> End
    tagColors :: TagKind -> TagColors
    tagColors = const $ TC.tagVar10


instance IsSortable TagKind Tag where
    kindOf :: Tag -> TagKind
    kindOf = const $ TagKind "tag"


instance IsGroupable Group Tag where
    t_group :: Tag -> Maybe (Chain Group)
    t_group = tagContent >>> map (\tag -> mkGroup [ PathSegment tag ] $ "#" <> tag) >>> Just -- TODO: use Group.cg constructors?


loadDecorators :: Achievement -> Decorators
loadDecorators = unwrap >>> \achRec ->
    Decorators.empty
    # (Decorators.put $ Dec.SProgress achRec.progress)
    # (case achRec.mbReference of
        Just groupPath -> Decorators.put $ Dec.SReference groupPath
        Nothing -> identity
       )
    # (case achRec.mbEarnedAt of
        Just dateEarned -> Decorators.put $ Dec.SEarnedAt dateEarned
        Nothing -> identity
       )
    # (case achRec.mbDescription of
        Just descr -> Decorators.put $ Dec.SDescription descr
        Nothing -> identity
       )


loadTags :: Achievement -> Tags Tag
loadTags = unwrap >>> _.tags >>> map Tag >>> Tags


applyDecorators :: Decorators -> Achievement -> Achievement
applyDecorators decorators = with \achRec ->
    Decorators.toArray decorators
        # foldl applyDecorator achRec
    where
        applyDecorator :: AchievementRec -> (Decorators.Key /\ Decorator) -> AchievementRec
        applyDecorator acc (key /\ decorator) = case decorator of
            Dec.SProgress prog -> acc { progress = prog }
            Dec.SEarnedAt sdate -> acc { mbEarnedAt = Just sdate }
            Dec.SDescription descr -> acc { mbDescription = Just descr }
            Dec.SReference gpath -> acc { mbReference = Just gpath }
            Dec.PRating _ -> acc
            Dec.PPriority _ -> acc
            Dec.PTask _ -> acc


applyTags :: Tags Tag -> Achievement -> Achievement
applyTags tags = with \achRec ->
    achRec { tags = unwrap <$> unwrap tags }


instance IsItem Achievement where
    i_title = unwrap >>> \achRec -> case achRec.mbTitle of
        Just title -> if achRec.name == "" then title else achRec.name <> " : " <> title
        Nothing -> achRec.name
    -- i_tags = unwrap >>> _.tags >>> map Tag


instance HasDecorators Achievement where
    i_decorators = loadDecorators


instance HasTags Tag Achievement where
    i_tags = unwrap >>> _.tags >>> map Tag


instance HasTabular Achievement where
    i_tabular = const Tabular.empty


instance ItemModify Achievement where
    setItemName newName = with \achRec -> achRec { name = newName }


instance DecoratorsModify Achievement where
    updateDecorators = applyDecorators


instance TagsModify Tag Achievement where
    updateTags = applyTags


bindToAchievement :: Achievement -> Group -> Group
bindToAchievement ach (Group group) = Group $ group { stats = SFromProgress $ getProgress ach }