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
import Report.Decorators.Stats (Stats)
import Report.Decorators.Class.ValueModify (fromEditable)
import Report.Decorators.Stats.Collect (collectStats, CollectWhat(..))
-- import Report.Prefix (Key, put) as Prefix
-- import Report.Prefix (Prefix, Prefixes)
-- import Report.Suffix (Key, put) as Suffix
-- import Report.Suffix (Suffix, Suffixes)
import Report.Decorator as Decorator


data WhatMod
    = PrefixMod Prefix.Key
    | SuffixMod Suffix.Key


data What
    = GroupName
    -- | GroupStat -- TODO
    | ItemName Int
    | ItemModifier Int WhatMod
    -- | AddPrefix -- TODO
    -- | AddSuffix -- TODO
    -- | AddItem -- TODO
    -- | AddGroup -- TODO


data WhatModKey
    = WKPrefix
    | WKSuffix


data WhatKey
    = WKGroupName
    -- | WKGroupStat -- TODO
    | WKItemName
    | WKItemModifier WhatModKey
    -- | WKAddPrefix -- TODO
    -- | WKAddSuffix -- TODO
    -- | WKAddItem -- TODO
    -- | WKAddGroup -- TODO


derive instance Eq WhatMod
derive instance Eq WhatModKey
derive instance Eq WhatKey


data Location subj_id
    = Nowhere
    | AtGroup    subj_id GroupPath
    | AtItem     subj_id GroupPath Int
    | AtModifier subj_id GroupPath Int WhatMod


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


class SuffixesModify t a where
    updateSuffixes :: Suffixes t -> a -> a


class PrefixesModify a where
    updatePrefixes :: Prefixes -> a -> a


class ItemModify a where
    setItemName :: String -> a -> a


loadModifierKey :: WhatMod -> WhatModKey
loadModifierKey = case _ of
    PrefixMod _ -> WKPrefix
    SuffixMod _ -> WKSuffix


loadPrefixKey :: WhatMod -> Maybe Prefix.Key
loadPrefixKey = case _ of
    PrefixMod prefixKey -> Just prefixKey
    SuffixMod _ -> Nothing


loadSuffixKey :: WhatMod -> Maybe Suffix.Key
loadSuffixKey = case _ of
    PrefixMod _ -> Nothing
    SuffixMod suffixKey -> Just suffixKey


modifyAt
    :: forall @tag subj_id subj group item
     . Eq subj_id
    => IsSubjectId subj_id subj
    => IsTag tag
    => IsGroup group
    => HasPrefixes item
    => HasSuffixes tag item
    => GroupModify group
    => ItemModify item
    => SuffixesModify tag item
    => PrefixesModify item
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
    ItemModifier itemIdx (PrefixMod pkey) -> do
        Report.withItem subjId path itemIdx (setPrefix pkey) report
    ItemModifier itemIdx (SuffixMod skey) -> do
        Report.withItem subjId path itemIdx (setSuffix skey) report
    where
        setPrefix :: Prefix.Key -> item -> item
        setPrefix pkey item =
            let
                (prefixes :: Prefixes) = i_prefixes item
                (mbDecodedValue :: Maybe Prefix) = fromEditable pkey newValue
                nextPrefixes = fromMaybe prefixes $ (\pfx -> Prefix.put pfx prefixes) <$> mbDecodedValue
            in updatePrefixes nextPrefixes item
        setSuffix :: Suffix.Key -> item -> item
        setSuffix skey item =
            let
                (suffixes :: Suffixes tag) = i_suffixes @tag item
                (mbDecodedValue :: Maybe (Suffix tag)) = fromEditable skey newValue
                nextSuffixes = fromMaybe suffixes $ (\sfx -> Suffix.put sfx suffixes) <$> mbDecodedValue
            in updateSuffixes nextSuffixes item
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
    :: forall @tag subj group item
     . Ord group
    => IsGroup group
    => HasSuffixes tag item
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
        updateGroup itemsCollected group = setStats (collectStats @tag cfg.collect itemsCollected) group
        updateGroups :: Array (group /\ Array item) -> Array (group /\ Array item)
        updateGroups groupsArr =
            groupsArr <#> \(group /\ items) ->
                case cfg.include of
                    AllNested ->
                        updateGroup (collectAllItems group groupsArr) group
                    OnlyDirect ->
                        updateGroup items group
                /\ items