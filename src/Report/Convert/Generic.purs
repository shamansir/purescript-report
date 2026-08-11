module Report.Convert.Generic where

import Prelude


import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map (toUnfoldable) as Map
import Data.Newtype (unwrap, wrap)
import Data.Tuple (curry, uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array (elem) as Array
import Data.String as String
import Data.String.CodePoints (codePointFromChar, toCodePointArray, fromCodePointArray) as CP
import Data.CodePoint.Unicode (isAlphaNum) as CP

import Yoga.JSON (writeImpl)

import Report (Report, class ToReport)
import Report.Group (Group(..))
import Report.Class
import Report.Chain (Chain)
import Report as Report
import Report.Builder as ReportB
import Report.Convert.Keyed (class EncodableKey, encodeKey)
import Report.Decorator (Decorator)
import Report.Decorator (Key) as Decorator
import Report.Tabular as Tabular
import Report.Decorators.Tabular.TabularValue (TabularValue)
-- import Report.Decorators.Tabular.TabularValue as TabV
import Report.Decorators.Tags (RawTag, RawTags(..))
import Report.Convert.Text.Decorators.Tags as CT
import Report.Convert.Types

import Report.Impl.Subject (Subject, SubjectId(..)) as Impl
import Report.Impl.Subject (mapTags, mapId, catMaybesOfTags, getId) as SubjImpl
import Report.Impl.Item (Item) as Impl
import Report.Impl.Item (mapTags, catMaybesOfTags) as ItemImpl
import Report.Impl.Group (Group) as Impl
-- import Report.Impl.Tag (Tag) as Impl


exportVersion = ExportVersion 3 :: ExportVersion

class
    ( Ord group
    , Eq subj_id
    , ConvertTo (Chain String) item_tag
    , ConvertTo (Chain String) subj_tag
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
    )
    <= ToExport subj_id subj_tag item_tag subj group item (x :: Type)


class
    ( Ord group
    , Eq subj_id
    , ConvertTo (Chain String) item_tag
    , ConvertTo (Chain String) subj_tag
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
    )
    <= ToExportX subj_id subj_tag item_tag subj group item



instance
    ( Ord group
    , Eq subj_id
    , ConvertTo (Chain String) item_tag
    , ConvertTo (Chain String) subj_tag
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
            { id : Impl.SubjectId $ encodeKey @subj_id $ s_id subj
            , name  : s_name  @subj_id subj
            , tags  : i_tags  @subj_tag subj <#> CT.rawifyTag
            , stats : i_stats subj
            -- , trackedAt : Nothing -- TODO
            -- , properties : [] -- collectModifiers @subj_tag subj
            -- , tabular : Tabular.empty
            , tabulars : collectTabulars subj
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
            , tags : RawTags $ CT.rawifyTag @item_tag <$> i_tags item
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
        -- collectTags :: forall @t a. ConvertFrom (Chain String) t => HasTags t a => a -> Array RawTag
        -- collectTags a =
        --     CT.rawifyTag @_ @t <$> i_tags a
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


type SubjectName = String


class ToImport subj_id subj_tag item_tag subj group item (x :: Type) where
    convertSubjectId :: Int -> SubjectName -> Maybe Impl.SubjectId -> Maybe subj_id -- FIXME: returning Nothing causes `subject` not to be imported
    convertSubjectTag :: RawTag -> Maybe subj_tag
    convertSubject :: Impl.Subject subj_id subj_tag -> Maybe subj
    convertGroup :: Impl.Group -> Maybe group
    convertItem :: Impl.Item item_tag -> Maybe item
    convertItemTag :: RawTag -> Maybe item_tag


{- Raw Report that just stores everything using `Impl.*` records, using basic types and strings wherever possible -}
type RawReportWith subj_id subj_tag item_tag = Report (Impl.Subject subj_id subj_tag) Impl.Group (Impl.Item item_tag)
type RawReportWith' subj_id = RawReportWith subj_id RawTag RawTag
type RawReport = RawReportWith Impl.SubjectId RawTag RawTag

newtype RR = RR RawReport
newtype RRX subj_id subj_tag item_tag (x :: Type) = RRX (RawReportWith subj_id subj_tag item_tag)


instance ToReport (Impl.Subject Impl.SubjectId RawTag) Impl.Group (Impl.Item RawTag) RR where
    toReport (RR report) = report


instance ToReport (Impl.Subject subj_id subj_tag) Impl.Group (Impl.Item item_tag) (RRX subj_id subj_tag item_tag x) where
    toReport (RRX report) = report


instance ToExport Impl.SubjectId RawTag RawTag (Impl.Subject Impl.SubjectId RawTag) Impl.Group (Impl.Item RawTag) RR


instance ToImport Impl.SubjectId RawTag RawTag (Impl.Subject Impl.SubjectId RawTag) Impl.Group (Impl.Item RawTag) RR where
    convertSubjectId :: Int -> SubjectName -> Maybe Impl.SubjectId -> Maybe Impl.SubjectId
    convertSubjectId _ name mbSubjId = Just $ fromMaybe (subjectIdFromName name) mbSubjId
    convertSubjectTag :: RawTag -> Maybe RawTag
    convertSubjectTag = Just
    convertSubject :: Impl.Subject Impl.SubjectId RawTag -> Maybe (Impl.Subject Impl.SubjectId RawTag)
    convertSubject = Just
    convertGroup :: Impl.Group -> Maybe Impl.Group
    convertGroup = Just
    convertItem :: Impl.Item RawTag -> Maybe (Impl.Item RawTag)
    convertItem = Just
    convertItemTag :: RawTag -> Maybe RawTag
    convertItemTag = Just


nameToId :: String -> String
nameToId name =
    String.toLower
         $ CP.fromCodePointArray
         $ (\cp -> if CP.isAlphaNum cp then cp else CP.codePointFromChar '-')
        <$> CP.toCodePointArray name


subjectIdFromName :: String -> Impl.SubjectId
subjectIdFromName name =
    Impl.SubjectId $ nameToId name


toImport :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => RawReport
    -> Report subj group item
toImport =
    Report.toBuilder
        >>> ReportB.mapSubjectsIndexed (\idx subj ->
                SubjImpl.getId subj
                     # Just
                     # convertSubjectId @subj_id @subj_tag @item_tag @subj @group @item @x idx (s_name @Impl.SubjectId subj)
                    <#> \subjId -> SubjImpl.mapId (const subjId) subj
            )
        >>> ReportB.catMaybesOfSubjects
        >>> Report.fromBuilder
        >>> toImport' @x @subj_id @subj_tag @item_tag @subj @group @item


toImport_ :: forall @x @subj_id @subj_tag @item_tag subj group item
     . ToImport subj_id subj_tag item_tag subj group item x
    => (Impl.SubjectId -> subj_id)
    -> RawReport
    -> Report subj group item
toImport_ toSubjId =
    Report.toBuilder
        >>> Report.mapSubjects (SubjImpl.mapId toSubjId)
        >>> Report.fromBuilder
        >>> toImport' @x @subj_id @subj_tag @item_tag @subj @group @item


toImport' :: forall @x @subj_id @subj_tag @item_tag @subj @group @item
     . ToImport subj_id subj_tag item_tag subj group item x
    => RawReportWith' subj_id
    -> Report subj group item
toImport' =
    Report.toBuilder
        >>> Report.mapItems
            ( ItemImpl.mapTags
                ( convertItemTag @subj_id @subj_tag @item_tag @subj @group @item @x )
                >>> ItemImpl.catMaybesOfTags
                >>> convertItem  @subj_id @subj_tag @item_tag @subj @group @item @x
            )
        >>> ReportB.catMaybesOfItems
        >>> Report.mapGroups
            ( convertGroup @subj_id @subj_tag @item_tag @subj @group @item @x )
        >>> ReportB.catMaybesOfGroups
        >>> Report.mapSubjects
            ( SubjImpl.mapTags
                ( convertSubjectTag @subj_id @subj_tag @item_tag @subj @group @item @x )
                >>> SubjImpl.catMaybesOfTags
                >>> convertSubject  @subj_id @subj_tag @item_tag @subj @group @item @x
            -- >>> Report.mapId (convertImpl.SubjectId @subj_id @subj_tag @item_tag @subj @group @item @x)
            )
        >>> ReportB.catMaybesOfSubjects
        >>> Report.fromBuilder


uniqueIdFromTitle :: String -> String
uniqueIdFromTitle = nameToId
    {-
    =   String.replaceAll (String.Pattern " ") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern ",") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern "(") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern ")") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern "'") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern "#") (String.Replacement "-")
    >>> String.replaceAll (String.Pattern "!") (String.Replacement "-") -}