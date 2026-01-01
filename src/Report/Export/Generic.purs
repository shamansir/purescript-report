module Report.Export.Generic where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Map.Extra (mapKeys) as Map
import Data.Newtype (unwrap)
import Data.Tuple (curry, uncurry) as Tuple

import Report (Report)
import Report.Group (Group(..))
import Report.Class
import Report (toMap) as Report
import Report.Modifiers.Class.ValueModify
import Report.Export.Types


toExport
    :: forall subj_id tag subj group item
     . Ord group
    => EncodableKey subj_id
    => IsTag tag
    => IsItem tag item
    => IsGroup group
    => IsSubject subj_id tag subj
    => Report subj group item
    -> ReportToExport
toExport =
    ReportToExport <<< map (Tuple.uncurry subjectToExport) <<< Map.toUnfoldable <<< Report.toMap
    where
        collectSubject :: subj -> SubjectRec
        collectSubject subj =
            { id : SubjectId $ encodeKey @subj_id $ s_id subj
            , name  : s_name  @subj_id @tag subj
            , tags  : s_tags  @subj_id @tag subj <#> tagContent
            , stats : s_stats @subj_id @tag subj
            , trackedAt : Nothing -- TODO
            , properties : [] -- TODO
            }
        collectGroup :: group -> Group
        collectGroup group =
            Group
                { title : g_title group
                , path  : g_path group
                , stats : g_stats group
                }
        collectItem :: item -> ItemRec
        collectItem item =
            { name : i_name @tag item
            , modifiers : []
                -- (?wh $ Map.mapKeys ?sh (unwrap $ i_prefixes item)) <>
                -- (?wh $ Map.mapKeys ?wh (unwrap $ i_suffixes item))
            , mbTitle : i_mbTitle @tag item
            , locked : i_locked @tag item
            }
        subjectToExport :: subj -> Map group (Array item) -> SubjectWithGroups
        subjectToExport subj groupsMap =
            SubjectWithGroups
                { subject : Subject $ collectSubject subj
                , groups  : groupsMap # Map.toUnfoldable # map (Tuple.uncurry groupToExport)
                }
        groupToExport :: group -> Array item -> { group :: Group, items :: Array ItemRec }
        groupToExport group items =
            { group : collectGroup group
            , items : items <#> collectItem
            }