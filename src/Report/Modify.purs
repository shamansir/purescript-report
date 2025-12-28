module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)

import Report.Core (EncodedValue(..))
import Report.Class (class IsGroup, g_stats, class IsSubjectId, s_id)
import Report.Prefix (Key) as Prefix
import Report.Suffix (Key) as Suffix
import Report.GroupPath (GroupPath)
import Report.Modifiers.Stats (Stats)
import Report (Report)
import Report (withGroup, withItem) as Report


data Where
    = GroupName
    -- | GroupStat -- TODO
    | ItemName Int
    | ItemPrefix Int Prefix.Key
    | ItemSuffix Int Suffix.Key
    -- | AddPrefix -- TODO
    -- | AddSuffix -- TODO
    -- | AddItem -- TODO
    -- | AddGroup -- TODO


data WhereKey
    = WKGroupName
    -- | WKGroupStat -- TODO
    | WKItemName
    | WKItemPrefix
    | WKItemSuffix
    -- | WKAddPrefix -- TODO
    -- | WKAddSuffix -- TODO
    -- | WKAddItem -- TODO
    -- | WKAddGroup -- TODO


derive instance Eq WhereKey


type Modification subj_id =
    { subjId :: subj_id
    , path :: GroupPath
    , where_ :: Where
    , newValue :: EncodedValue
    }


class GroupModify group where
    setGroupName :: String -> group -> group
    setGroupStats :: Stats -> group -> group


modifyAt :: forall subj group item. Ord subj => IsGroup group => GroupModify group => Modification subj -> Report subj group item -> Maybe (Report subj group item)
modifyAt { subjId, where_, newValue, path } report = case where_ of
    GroupName -> do
        Report.withGroup subj path (setGroupName $ unwrapEditable newValue) report
    -- GroupStat -> do
    --     Report.withGroup subj path (\group -> setGroupStats (groupStatsFromEditable newValue group) group) report
    ItemName itemIdx -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report
    ItemPrefix itemIdx pkey -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report
    ItemSuffix itemIdx skey -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report
    where
        subj = subjId
        unwrapEditable (EncodedValue string) = string
        -- groupStatsFromEditable :: Editable -> group -> Stats
        -- groupStatsFromEditable editable group = fromMaybe (g_stats group) $ fromEditable editable