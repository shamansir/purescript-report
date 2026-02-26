module Report.Convert.Generic where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd, curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (filter, elem) as Array

import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

import Report (Report, class ToReport)
import Report.Group (Group(..))
import Report.Class
import Report as Report
import Report.Builder as ReportB
import Report.Chain as MbW
import Report.Convert.Keyed (class EncodableKey, encodeKey)
import Report.Decorator (Decorator, Key)
import Report.Tabular as Tabular
import Report.Convert.Types


exportVersion = ExportVersion 3 :: ExportVersion

class
    ( Ord group
    , Eq subj_id
    , IsTag subj_tag
    , IsItem item
    , IsGroup group
    , IsSubject subj_id subj
    , HasTags subj_tag subj
    , HasDecorators item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
    , EncodableKey subj_id
    , WriteForeign item_tag
    , ReadForeign item_tag
    , WriteForeign subj_tag
    -- => WriteForeign subj_tag
    , ToReport subj group item x
    )
    <= ToExport subj_id subj_tag item_tag subj group item (x :: Type)



instance
    ( Ord group
    , Eq subj_id
    , IsTag subj_tag
    , IsItem item
    , IsGroup group
    , IsSubject subj_id subj
    , HasTags subj_tag subj
    , HasDecorators item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
    , EncodableKey subj_id
    , WriteForeign item_tag
    , ReadForeign item_tag
    , WriteForeign subj_tag
    -- => WriteForeign subj_tag
    , ToReport subj group item x
    )
    => ToExport subj_id subj_tag item_tag subj group item (Report subj group item)


data IncludeRule subj_id
    = IncludeAll
    | IncludeOnly (Array subj_id)


includeAll = IncludeAll :: forall subj_id. IncludeRule subj_id
includeOnly = IncludeOnly :: forall subj_id. Array subj_id -> IncludeRule subj_id


toExport
    :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToExport subj_id subj_tag item_tag subj group item x
    => IncludeRule subj_id
    -> Report subj group item
    -> ReportToExport
toExport inclRule =
    ReportToExport
        <<< (\subjects -> { version : exportVersion, subjects })
        <<< map (Tuple.uncurry subjectToExport)
        <<< ReportB.unfold
        <<< ReportB.mapItems collectItem
        <<< ReportB.mapGroups collectGroup
        <<< ReportB.mapSubjects collectSubject
        <<< ReportB.filterSubjects (\subj ->
                case inclRule of
                    IncludeAll -> true
                    IncludeOnly ids -> Array.elem (s_id subj) ids
                )
        <<< Report.toBuilder
    where
        collectSubject :: subj -> SubjectRec
        collectSubject subj =
            { id : SubjectId $ encodeKey @subj_id $ s_id subj
            , name  : s_name  @subj_id subj
            , tags  : i_tags  @subj_tag subj <#> tagContent <#> MbW.toString
            , stats : i_stats subj
            -- , trackedAt : Nothing -- TODO
            -- , properties : [] -- collectModifiers @subj_tag subj
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
        collectModifiers :: forall @t a. WriteForeign t => HasDecorators t a => a -> Array ModifierRec
        -- collectModifiers a = []
        -- collectModifiers a = Tuple.uncurry collectPrefix      <$> (Map.toUnfoldable $ unwrap $ i_prefixes a)
        collectModifiers a =
            (Tuple.uncurry collectPrefix      <$> (Map.toUnfoldable $ unwrap $ i_prefixes a)) <>
            (Tuple.uncurry (collectSuffix @t) <$> (Map.toUnfoldable $ unwrap $ i_suffixes @t a))
        subjectToExport :: SubjectRec -> Array (Group /\ Array ItemRec) -> SubjectWithGroups
        subjectToExport subjRec groups =
            SubjectWithGroups
                { subject : Subject subjRec
                , groups  : Tuple.uncurry groupToExport <$> groups
                }
        groupToExport :: Group -> Array ItemRec -> { group :: Group, items :: Array ItemRec }
        groupToExport group items = { group, items }