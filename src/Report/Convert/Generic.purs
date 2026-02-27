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
import Report.Decorator (Key, mapTags) as Decorator
import Report.Decorators.Tags (RawTag)
import Report.Tabular as Tabular
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
    , HasDecorators item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
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
    , HasDecorators item_tag item
    , HasTabular subj
    , HasStats subj
    , HasStats group
    , EncodableKey subj_id
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
            { name : i_name item
            , decorators : collectDecorators @item_tag item
            , mbTitle : i_mbTitle item
            , locked  : i_locked  item
            }
        collectDecorator :: Decorator.Key -> Decorator RawTag -> DecoratorRec
        collectDecorator dkey decorator =
            { mkey  : encodeKey dkey
            , value : writeImpl decorator -- ValueModify.toEditable
            }
        collectDecorators :: forall @t a. IsTag t => HasDecorators t a => a -> Array DecoratorRec
        collectDecorators a =
            -- []
            (Tuple.uncurry collectDecorator <$> map (rawifyDecorator @t) <$> (Map.toUnfoldable $ unwrap $ i_decorators @t a))
        rawifyDecorator :: forall @t. IsTag t => Decorator t -> Decorator RawTag
        rawifyDecorator = Decorator.mapTags (rawifyTag @t)
        subjectToExport :: SubjectRec -> Array (Group /\ Array ItemRec) -> SubjectWithGroups
        subjectToExport subjRec groups =
            SubjectWithGroups
                { subject : Subject subjRec
                , groups  : Tuple.uncurry groupToExport <$> groups
                }
        groupToExport :: Group -> Array ItemRec -> { group :: Group, items :: Array ItemRec }
        groupToExport group items = { group, items }