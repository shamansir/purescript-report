module Report.Modify where

import Prelude

import Data.Maybe (Maybe, fromMaybe)

import Report (Report)
import Report (withGroup, withItem) as Report
import Report.Class (class IsGroup, class IsSubjectId, class IsItem, class IsTag, i_suffixes)
import Report.Core.Logic (EncodedValue(..))
import Report.GroupPath (GroupPath)
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Class.ValueModify (fromEditable)
import Report.Prefix (Key) as Prefix
import Report.Prefix (Prefixes)
import Report.Suffix (Key, put) as Suffix
import Report.Suffix (Suffix, Suffixes)


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
    :: forall @tag subj_id subj group item
     . Ord subj
    => Ord subj_id
    => IsSubjectId subj_id subj
    => IsTag tag
    => IsGroup group
    => IsItem tag item
    => GroupModify group
    => ItemModify tag item
    => Modification subj_id
    -> Report subj group item
    -> Maybe (Report subj group item)
modifyAt { subjId, what, newValue, path } report = case what of
    GroupName -> do
        Report.withGroup subjId path (setGroupName $ unwrapEditable newValue) report
    -- GroupStat -> do
    --     Report.withGroup subj path (\group -> setGroupStats (groupStatsFromEditable newValue group) group) report
    ItemName itemIdx -> do
        Report.withItem subjId path itemIdx (setName @tag $ unwrapEditable newValue) report
    ItemPrefix itemIdx pkey -> do
        Report.withItem subjId path itemIdx (identity {- TODO -}) report
    ItemSuffix itemIdx skey -> do
        Report.withItem subjId path itemIdx (setSuffix skey) report
    where
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