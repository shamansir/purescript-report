module Report.Export.Generic where

import Prelude

import Foreign (Foreign)

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Map.Extra (mapKeys) as Map
import Data.Newtype (unwrap)
import Data.Tuple (curry, uncurry) as Tuple

import Yoga.JSON (class WriteForeign, writeImpl)

import Report (Report)
import Report.Group (Group(..))
import Report.Class
import Report (toMap) as Report
import Report.Modifiers.Class.ValueModify
import Report.Prefix (Prefix)
import Report.Prefix (Key) as Prefix
import Report.Suffix (Suffix)
import Report.Suffix (Key) as Suffix
import Report.Export.Types


toExport
    :: forall @subj_id @subj_tag @item_tag subj group item
     . Ord group
    => EncodableKey subj_id
    => WriteForeign item_tag
    => IsTag subj_tag
    => IsItem item_tag item
    => IsGroup group
    => IsSubject subj_id subj_tag subj
    => Report subj group item
    -> ReportToExport
toExport =
    ReportToExport <<< map (Tuple.uncurry subjectToExport) <<< Map.toUnfoldable <<< Report.toMap
    where
        collectSubject :: subj -> SubjectRec
        collectSubject subj =
            { id : SubjectId $ encodeKey @subj_id $ s_id subj
            , name  : s_name  @subj_id @subj_tag subj
            , tags  : s_tags  @subj_id @subj_tag subj <#> tagContent
            , stats : s_stats @subj_id @subj_tag subj
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
            { name : i_name @item_tag item
            , modifiers :
                (Tuple.uncurry collectPrefix <$> (Map.toUnfoldable $ unwrap $ i_prefixes @item_tag item)) <>
                (Tuple.uncurry collectSuffix <$> (Map.toUnfoldable $ unwrap $ i_suffixes @item_tag item))
            , mbTitle : i_mbTitle @item_tag item
            , locked  : i_locked  @item_tag item
            }
        collectPrefix :: Prefix.Key -> Prefix -> ModifierRec
        collectPrefix pkey prefix =
            { mkey  : encodeKey pkey
            , mkind : P
            , value : writeImpl prefix -- ValueModify.toEditable
            }
        collectSuffix :: Suffix.Key -> Suffix item_tag -> ModifierRec
        collectSuffix skey suffix =
            { mkey  : encodeKey skey
            , mkind : S
            , value : writeImpl suffix -- ValueModify.toEditable
            }
        subjectToExport :: subj -> Map group (Array item) -> SubjectWithGroups
        subjectToExport subj groupsMap =
            SubjectWithGroups
                { subject : Subject $ collectSubject subj
                , groups  : groupsMap # Map.toUnfoldable <#> Tuple.uncurry groupToExport
                }
        groupToExport :: group -> Array item -> { group :: Group, items :: Array ItemRec }
        groupToExport group items =
            { group : collectGroup group
            , items : items <#> collectItem
            }