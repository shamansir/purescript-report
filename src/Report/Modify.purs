module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:))
import Data.Array (index, catMaybes, snoc, updateAt, concat, any, filter, concat) as Array
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, build, node, leaf) as Tree

import Report (Report)
import Report.Builder (TreeNode(..))
import Report.Builder as RBuilder
import Report (toBuilder, fromBuilder, withGroup, withItem, toTree) as Report
import Report.Class
import Report.Chain (Chain)
import Report.Chain as Chain
import Report.Core.Logic (EncodedValue(..))
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq, pathFromArray, startsWith, encode) as GPath
import Report.Decorator as Decorator
import Report.Decorator (Decorator, Decorators)
import Report.Decorators.Stats (Stats)
import Report.Decorators.Tags (Tags, RawTag)
import Report.Decorators.Tags (fromArray) as Tags
import Report.Decorators.Class.ValueModify (fromEditable)
import Report.Decorators.Stats.Collect (collectStats, CollectWhat)


data What
    = GroupName
    -- | GroupStat -- TODO
    | ItemName Int
    | ItemDecorator Int Decorator.Key
    | ItemTag Int Int
    | ItemTabular Int Int
    -- | AddDecorator -- TODO
    -- | AddItem -- TODO
    -- | AddGroup -- TODO


data WhatKey
    = WKGroupName
    -- | WKGroupStat -- TODO
    | WKItemName
    | WKItemDecorator
    | WKItemTags
    | WKItemTabular
    -- | WKAddDecorator -- TODO
    -- | WKAddItem -- TODO
    -- | WKAddGroup -- TODO


derive instance Eq WhatKey


data Location subj_id
    = Nowhere
    | AtGroup     subj_id GroupPath
    | AtItem      subj_id GroupPath Int
    | AtDecorator subj_id GroupPath Int Decorator.Key
    | AtTag       subj_id GroupPath Int Int
    | AtTabular   subj_id GroupPath Int Int


type Modification subj_id =
    { subjId :: subj_id
    , path :: GroupPath
    , what :: What
    , newValue :: EncodedValue
    }


class GroupModify group where
    setGroupName :: String -> group -> group


class StatsModify a where
    setStats :: Stats -> a -> a


class DecoratorsModify a where
    updateDecorators :: Decorators -> a -> a


class TagsModify t a where
    updateTags :: Tags t -> a -> a


class ItemModify a where
    setItemName :: String -> a -> a


modifyAt
    :: forall @tag subj_id subj group item
     . Eq subj_id
    => IsSubjectId subj_id subj
    => IsTag tag
    => IsGroup group
    => HasDecorators item
    => HasTags tag item
    => GroupModify group
    => ItemModify item
    => TagsModify tag item
    => DecoratorsModify item
    => Modification subj_id
    -> Report subj group item
    -> Report subj group item
modifyAt { subjId, what, newValue, path } report = case what of
    GroupName -> do
        Report.withGroup subjId path (setGroupName $ unwrapEditable newValue) report
    -- GroupStat -> do
    --     Report.withGroup subj path (\group -> setGroupStats (groupStatsFromEditable newValue group) group) report
    ItemName itemIdx -> do
        Report.withItem subjId path itemIdx (setItemName $ unwrapEditable newValue) report
    ItemDecorator itemIdx deckey -> do
        Report.withItem subjId path itemIdx (setDecorator deckey) report
    ItemTag itemIdx tagIdx ->
        Report.withItem subjId path itemIdx setTags report
    ItemTabular itemIdx tabularIdx ->
        report -- FIXME: Implement
    where
        -- setDecorator :: Decorator.Key -> item -> item
        -- setDecorator pkey item =
        --     let
        --         (prefixes :: Prefixes) = i_prefixes item
        --         (mbDecodedValue :: Maybe Prefix) = fromEditable pkey newValue
        --         nextPrefixes = fromMaybe prefixes $ (\pfx -> Prefix.put pfx prefixes) <$> mbDecodedValue
        --     in updatePrefixes nextPrefixes item
        setDecorator :: Decorator.Key -> item -> item
        setDecorator skey item =
            let
                (decorators :: Decorators) = i_decorators item
                (mbDecodedValue :: Maybe Decorator) = fromEditable skey newValue
                nextDecorators = fromMaybe decorators $ (\dec -> Decorator.put dec decorators) <$> mbDecodedValue
            in updateDecorators nextDecorators item
        setTags :: item -> item
        setTags item =
            let
                (currentTags :: Tags tag) = Tags.fromArray $ i_tags item
                (mbDecodedValue :: Maybe (Tags RawTag)) = fromEditable unit newValue
                nextTags = fromMaybe currentTags $ (\tags -> derawifyTags @tag tags) <$> mbDecodedValue
            in updateTags nextTags item
        unwrapEditable (EncodedValue string) = string
        -- groupStatsFromEditable :: Editable -> group -> Stats
        -- groupStatsFromEditable editable group = fromMaybe (g_stats group) $ fromEditable editable


data RecalculateInclude
    = OnlyDirect
    | AllNested


{-
recalculate
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => Report subj group item
    -> Report subj group item
recalculate =
    recalculate_ @tag AllNested ItemsProgress


recalculateDirect
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => Report subj group item
    -> Report subj group item
recalculateDirect =
    recalculate_ @tag OnlyDirect ItemsProgress
-}


type RecalculateConfig =
    { include :: RecalculateInclude
    , collect :: CollectWhat
    }


{-
recalculateAlt
    :: forall @tag subj group item
     . IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => RecalculateConfig
    -> Report subj group item
    -> Report subj group item
recalculateAlt cfg =
    Report.toBuilder >>> RBuilder.unfoldC >>> (map $ map updateGroups) >>> RBuilder.toBuilderC >>> Report.fromBuilder   -- FIXME: TODO!
    where
        belongsTo :: Chain group -> Chain group -> Boolean
        belongsTo grpCA grpCB = Debug.spy "startsWith" $ GPath.startsWith (g_path $ Chain.last $ spyGroupC "grpCA" $ grpCA) (g_path $ Chain.last $ spyGroupC "grpCB" $ grpCB)
        collectAllItems :: Chain group -> Array (Chain group /\ Array item) -> Array item
        collectAllItems grpC = Array.filter (Tuple.fst >>> belongsTo grpC) >>> map Tuple.snd >>> Array.concat
        spyGroup :: String -> group -> group
        spyGroup label = Debug.spyWith label (g_path >>> GPath.encode)
        spyGroupC :: String -> Chain group -> Chain group
        spyGroupC label = Debug.spyWith label (map (g_path >>> GPath.encode) >>> Chain.toString)
        updateGroup :: Array item -> group -> group
        updateGroup itemsCollected group = setStats (collectStats @tag cfg.collect itemsCollected # Debug.spy "stats") $ spyGroup "group" group
        updateGroups :: Array (Chain group /\ Array item) -> Array (Chain group /\ Array item)
        updateGroups groupsArr =
            groupsArr <#> \(groupC /\ items) ->
                case cfg.include of
                    AllNested ->
                        updateGroup (collectAllItems groupC groupsArr) <$> groupC
                    OnlyDirect ->
                        updateGroup items <$> groupC
                /\ items
-}

recalculate
    :: forall subj group item
     . Ord group
    => IsGroup group
    => HasDecorators item
    => StatsModify group
    => RecalculateConfig
    -> Report subj group item
    -> Report subj group item
recalculate cfg =
    -- FIXME: check if group chains properly go back!
    Report.toBuilder >>> RBuilder.unfoldAll >>> (map $ map updateGroups) >>> RBuilder.toBuilder >>> Report.fromBuilder
    where
        belongsTo :: group -> group -> Boolean
        belongsTo grpA grpB = GPath.startsWith (g_path grpA) (g_path grpB)
        collectAllItems :: group -> Array (group /\ Array item) -> Array item
        collectAllItems grp = Array.filter (Tuple.fst >>> belongsTo grp) >>> map Tuple.snd >>> Array.concat
        updateGroup :: Array item -> group -> group
        updateGroup itemsCollected group = setStats (collectStats cfg.collect itemsCollected) group
        updateGroups :: Array (group /\ Array item) -> Array (group /\ Array item)
        updateGroups groupsArr =
            groupsArr <#> \(group /\ items) ->
                case cfg.include of
                    AllNested ->
                        updateGroup (collectAllItems group groupsArr) group
                    OnlyDirect ->
                        updateGroup items group
                /\ items


-- loadDecoratorKey :: What -> Maybe Decorator.Key
-- loadDecoratorKey = case _ of
--     ItemDecorator _ deckey -> Just deckey
--     _ -> Nothing