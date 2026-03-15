module Report.Convert.Generic where

import Prelude

import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap)
import Data.Tuple (curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (filter, elem) as Array

import Yoga.JSON (writeImpl)

import Report (Report, class ToReport)
import Report.Group (Group(..))
import Report.Class
import Report as Report
import Report.Builder as ReportB
import Report.Convert.Keyed (class EncodableKey, encodeKey)
import Report.Decorator (Decorator)
import Report.Decorator (Key) as Decorator
import Report.Decorators.Tags (RawTag)
import Report.Tabular as Tabular
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Decorators.Tabular.TabularValue as TabV
import Report.Convert.Types


exportVersion = ExportVersion 3 :: ExportVersion

class
    ( Ord group
    , Eq subj_id
    , IsTag subj_tag
    , IsTag item_tag
    , IsItem item
    , IsGroup group
    , IsSubject subj_id subj
    , HasTags subj_tag subj
    , HasDecorators item
    , HasTags item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
    , HasTabular item
    , EncodableKey subj_id
    -- => WriteForeign subj_tag
    , ToReport subj group item x
    )
    <= ToExport subj_id subj_tag item_tag subj group item (x :: Type)



instance
    ( Ord group
    , Eq subj_id
    , IsTag subj_tag
    , IsTag item_tag
    , IsItem item
    , IsGroup group
    , IsSubject subj_id subj
    , HasTags subj_tag subj
    , HasDecorators item
    , HasTags item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
    , EncodableKey subj_id
    , HasTabular item
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
            , tags  : i_tags  @subj_tag subj <#> rawifyTag
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
            { title : i_title item
            , decorators : collectDecorators item
            , tags : collectTags @item_tag item
            , tabulars : collectTabulars item
            }
        collectDecorator :: Decorator.Key -> Decorator -> DecoratorRec
        collectDecorator dkey decorator =
            { mkey  : encodeKey dkey
            , fvalue : writeImpl decorator -- ValueModify.toEditable
            }
        collectDecorators :: forall a. HasDecorators a => a -> Array DecoratorRec
        collectDecorators a =
            -- []
            (Tuple.uncurry collectDecorator <$> (Map.toUnfoldable $ unwrap $ i_decorators a))
        collectTabular :: Tabular.Item TabularValue -> TabularRec
        collectTabular = case _ of
            Tabular.Item { key, label, value } ->
                { tkey  : key
                , tlabel : label
                , value : value -- ValueModify.toEditable
                }
        collectTabulars :: forall a. HasTabular a => a -> Array TabularRec
        collectTabulars a =
            -- []
            collectTabular <$> (Tabular.items $ i_tabular a)
        collectTags :: forall @t a. IsTag t => HasTags t a => a -> Array RawTag
        collectTags a =
            rawifyTag @t <$> i_tags a
        -- rawifyDecorator :: forall @t. IsTag t => Decorator t -> Decorator RawTag
        -- rawifyDecorator = Decorator.mapTags (rawifyTag @t)
        subjectToExport :: SubjectRec -> Array (Group /\ Array ItemRec) -> SubjectWithGroups
        subjectToExport subjRec groups =
            SubjectWithGroups
                { subject : Subject subjRec
                , groups  : Tuple.uncurry groupToExport <$> groups
                }
        groupToExport :: Group -> Array ItemRec -> { group :: Group, items :: Array ItemRec }
        groupToExport group items = { group, items }