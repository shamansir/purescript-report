module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

import Report.Core (EncodedValue(..))
import Report.Class (class IsGroup, g_stats, class IsSubjectId, s_id)
import Report.Prefix (Key) as Prefix
import Report.Suffix (Key) as Suffix
import Report.Prefix (Prefixes)
import Report.Suffix (Suffixes)
import Report.GroupPath (GroupPath)
import Report.Modifiers.Stats (Stats)
import Report (Report)
import Report (withGroup, withItem) as Report


data What
    = GroupName
    -- | GroupStat -- TODO
    | ItemName Int
    | ItemPrefix Int Prefix.Key
    | ItemSuffix Int Suffix.Key
    -- | AddPrefix -- TODO
    -- | AddSuffix -- TODO
    -- | AddItem -- TODO
    -- | AddGroup -- TODO


data WhatKey
    = WKGroupName
    -- | WKGroupStat -- TODO
    | WKItemName
    | WKItemPrefix
    | WKItemSuffix
    -- | WKAddPrefix -- TODO
    -- | WKAddSuffix -- TODO
    -- | WKAddItem -- TODO
    -- | WKAddGroup -- TODO


derive instance Eq WhatKey


data Location subj_id
    = Nowhere
    | AtGroup  subj_id GroupPath
    | AtItem   subj_id GroupPath Int
    | AtSuffix subj_id GroupPath Int Suffix.Key


type Modification subj_id =
    { subjId :: subj_id
    , path :: GroupPath
    , what :: What
    , newValue :: EncodedValue
    }


class GroupModify group where
    setGroupName :: String -> group -> group
    setGroupStats :: Stats -> group -> group


class ItemModify t a where
    setName :: String -> a -> a
    updateSuffixes :: Suffixes t -> a -> a
    updatePrefixes :: Prefixes -> a -> a


modifyAt
    :: forall subj_id subj group item
     . Ord subj
    => Ord subj_id
    => IsSubjectId subj_id subj
    => IsGroup group
    => GroupModify group
    => Modification subj_id
    -> Report subj group item
    -> Maybe (Report subj group item)
modifyAt { subjId, what, newValue, path } report = case what of
    GroupName -> do
        Report.withGroup subjId path (setGroupName $ unwrapEditable newValue) report
    -- GroupStat -> do
    --     Report.withGroup subj path (\group -> setGroupStats (groupStatsFromEditable newValue group) group) report
    ItemName itemIdx -> do
        Report.withItem subjId path itemIdx (identity {- TODO -}) report
    ItemPrefix itemIdx pkey -> do
        Report.withItem subjId path itemIdx (identity {- TODO -}) report
    ItemSuffix itemIdx skey -> do
        Report.withItem subjId path itemIdx (identity {- TODO -}) report
    where
        unwrapEditable (EncodedValue string) = string
        -- groupStatsFromEditable :: Editable -> group -> Stats
        -- groupStatsFromEditable editable group = fromMaybe (g_stats group) $ fromEditable editable