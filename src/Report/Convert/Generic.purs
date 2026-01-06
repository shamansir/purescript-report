module Report.Convert.Generic where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple (curry, uncurry) as Tuple

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

import Report (Report, class ToReport)
import Report.Group (Group(..))
import Report.Class
import Report (toMap) as Report
import Report.Convert.Keyed (class EncodableKey, encodeKey)
import Report.Prefix (Prefix)
import Report.Prefix (Key) as Prefix
import Report.Suffix (Suffix)
import Report.Suffix (Key) as Suffix
import Report.Tabular as Tabular
import Report.Convert.Types


exportVersion = ExportVersion 2 :: ExportVersion

class
    ( Ord group -- V
    , IsTag subj_tag -- V
    , IsItem item -- V
    , IsGroup group -- V
    , IsSubject subj_id subj -- V
    , HasTags subj_tag subj -- V
    , HasPrefixes item -- V
    , HasSuffixes item_tag item -- V
    , HasTabular subj
    , HasStats subj -- V
    , HasStats group -- V
    , EncodableKey subj_id -- V
    , WriteForeign item_tag -- V
    , ReadForeign item_tag
    -- => WriteForeign subj_tag
    , ToReport subj group item x
    )
    <= ToExport subj_id subj_tag item_tag subj group item (x :: Type)


toExport
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToExport subj_id subj_tag item_tag subj group item x
    => Report subj group item
    -> ReportToExport
toExport =
    ReportToExport
        <<< (\subjects -> { version : exportVersion, subjects })
        <<< map (Tuple.uncurry subjectToExport)
        <<< Map.toUnfoldable
        <<< Report.toMap
    where
        collectSubject :: subj -> SubjectRec
        collectSubject subj =
            { id : SubjectId $ encodeKey @subj_id $ s_id subj
            , name  : s_name  @subj_id subj
            , tags  : i_tags  @subj_tag subj <#> tagContent
            , stats : i_stats subj
            , trackedAt : Nothing -- TODO
            , properties : [] -- TODO
            -- , tabular : Tabular.empty
            , tabular : i_tabular subj
            }
        collectGroup :: group -> Group
        collectGroup group =
            Group
                { title : g_title group
                , path  : g_path group
                , stats : i_stats group
                }
        collectItem :: item -> ItemRec
        collectItem item =
            { name : i_name item
            , modifiers : collectModifiers @item_tag item
            , mbTitle : i_mbTitle item
            , locked  : i_locked  item
            }
        collectPrefix :: Prefix.Key -> Prefix -> ModifierRec
        collectPrefix pkey prefix =
            { mkey  : encodeKey pkey
            , mkind : P
            , value : writeImpl prefix -- ValueModify.toEditable
            }
        collectSuffix :: forall @t. WriteForeign t => Suffix.Key -> Suffix t -> ModifierRec
        collectSuffix skey suffix =
            { mkey  : encodeKey skey
            , mkind : S
            , value : writeImpl suffix -- ValueModify.toEditable
            }
        collectModifiers :: forall @t a. WriteForeign t => HasPrefixes a => HasSuffixes t a => a -> Array ModifierRec
        -- collectModifiers a = []
        -- collectModifiers a = Tuple.uncurry collectPrefix      <$> (Map.toUnfoldable $ unwrap $ i_prefixes a)
        collectModifiers a =
            (Tuple.uncurry collectPrefix      <$> (Map.toUnfoldable $ unwrap $ i_prefixes a)) <>
            (Tuple.uncurry (collectSuffix @t) <$> (Map.toUnfoldable $ unwrap $ i_suffixes @t a))
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