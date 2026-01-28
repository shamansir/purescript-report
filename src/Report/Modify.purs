module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array ((:))
import Data.Array (index, catMaybes, snoc, updateAt, concat) as Array

import Yoga.Tree (Tree)
import Yoga.Tree.Extended (break, build, node, leaf) as Tree

import Report (Report)
import Report.Builder (TreeNode(..))
import Report (withGroup, withItem, toTree, fromTree) as Report
import Report.Class
import Report.Core.Logic (EncodedValue(..))
import Report.GroupPath (GroupPath)
import Report.GroupPath (howDeep, startsWithNotEq, pathFromArray) as GPath
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Class.ValueModify (fromEditable)
import Report.Modifiers.Stats.Collect (collectStats)
import Report.Prefix (Key, put) as Prefix
import Report.Prefix (Prefix, Prefixes)
import Report.Suffix (Key, put) as Suffix
import Report.Suffix (Suffix, Suffixes)



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
     . Ord subj
    => Ord subj_id
    => IsSubjectId subj_id subj
    => IsTag tag
    => IsGroup group
    => IsItem item
    => HasPrefixes item
    => HasSuffixes tag item
    => GroupModify group
    => ItemModify item
    => SuffixesModify tag item
    => PrefixesModify item
    => Modification subj_id
    -> Report subj group item
    -> Maybe (Report subj group item)
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


recalculate
    :: forall @tag subj group item
     . Ord subj
    => Ord group
    => IsItem item
    => IsGroup group
    => HasSuffixes tag item
    => StatsModify group
    => GroupModify group
    => Report subj group item
    -> Report subj group item
recalculate = Report.toTree >>> updateTree >>> Report.fromTree identity
    where
        updateTree :: Tree (TreeNode subj group item) -> Tree (TreeNode subj group item)
        updateTree = Tree.break \node children ->
            case node of
                NRoot -> Tree.node NRoot $ map updateTree children
                NSubject subj -> Tree.node (NSubject subj) $ map updateTree children
                NGroup group ->
                    let
                         -- FIXME: we collect children as deep as we can, but we don't collect stats from subgroups yet
                        itemsCollected = Array.concat $ Tree.break collectItems <$> children
                        updatedChildren = map updateTree children
                        updatedGroup = setStats (collectStats @tag itemsCollected) group
                    in Tree.node (NGroup updatedGroup) updatedChildren
                NItem item -> Tree.leaf $ NItem item
        collectItems :: TreeNode subj group item -> Array (Tree (TreeNode subj group item)) -> Array item
        collectItems node children = case node of
            NItem item -> item : (Array.concat $ Tree.break collectItems <$> children)
            _ -> Array.concat $ Tree.break collectItems <$> children