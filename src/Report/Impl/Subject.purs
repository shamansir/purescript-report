module Report.Impl.Subject where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Chain (Chain)
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Stats (Stats(..)) as Stats
import Report.Modifiers.Tabular.TabularValue (TabularValue)


import Yoga.JSON (class WriteForeign, class ReadForeign)


type SubjectRec subj_id subj_tag =
    { name :: String
    , id :: subj_id
    -- , properties :: Array DhallProperty
    , stats :: Stats
    , tags :: Array subj_tag
    , tabular :: Tabular TabularValue
    }


newtype Subject subj_id subj_tag = Subject (SubjectRec subj_id subj_tag)
derive instance Newtype (Subject subj_id subj_tag) _


instance Eq subj_id => Eq (Subject subj_id subj_tag) where
    eq a b = eq (unwrap a # _.id) (unwrap b # _.id)


instance Ord subj_id => Ord (Subject subj_id subj_tag) where
    compare a b = compare (unwrap a # _.id) (unwrap b # _.id)


instance IsSubjectId String (Subject String subj_tag) where
    s_id = _.id <<< unwrap
    s_unique = identity


instance IsSubjectId Int (Subject Int subj_tag) where
    s_id = _.id <<< unwrap
    s_unique = show


instance (IsSubjectId subj_id (Subject subj_id subj_tag)) => IsSubject subj_id (Subject subj_id subj_tag) where
    s_name = _.name <<< unwrap


instance HasTags subj_tag (Subject subj_id subj_tag) where
    i_tags = _.tags <<< unwrap


instance HasTabular (Subject subj_id subj_tag) where
    i_tabular = _.tabular <<< unwrap


instance HasStats (Subject subj_id subj_tag) where
    i_stats = _.stats <<< unwrap


init :: forall subj_id subj_tag. subj_id -> String -> Subject subj_id subj_tag
init subj_id name =
    Subject
        { name
        , id : subj_id
        , tags : []
        , stats : Stats.SYetUnknown
        , tabular : Tabular.empty
        }


from
    :: forall subj_id subj_tag subj.
    IsSubjectId subj_id subj =>
    IsSubject subj_id subj =>
    HasTags subj_tag subj =>
    HasStats subj =>
    HasTabular subj =>
    subj ->
    Subject subj_id subj_tag
from subj =
    Subject
        { name : s_name @subj_id subj
        , id : s_id subj
        , tags : i_tags subj
        , stats : i_stats subj
        , tabular : i_tabular subj
        }


derive newtype instance (ReadForeign subj_id)  => ReadForeign  (Subject subj_id String)
derive newtype instance (WriteForeign subj_id) => WriteForeign (Subject subj_id String)