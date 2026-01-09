module Report.Impl.Subject where

import Prelude

import Data.Newtype (class Newtype, wrap, unwrap)

import Report.Class
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Modifiers.Tabular.TabularValue (TabularValue)


import Yoga.JSON (class WriteForeign, class ReadForeign)


type SubjectRec subj_id subj_tag =
    { name :: String
    , id :: subj_id
    -- , properties :: Array DhallProperty
    , tags :: Array subj_tag
    , tabular :: Tabular TabularValue
    }


newtype Subject subj_id subj_tag = Subject (SubjectRec subj_id subj_tag)
derive instance Newtype (Subject subj_id subj_tag) _


instance IsSubjectId String (Subject String subj_tag) where
    s_id = _.id <<< unwrap
    s_unique = identity


instance IsSubjectId Int (Subject Int subj_tag) where
    s_id = _.id <<< unwrap
    s_unique = show


instance (IsSubjectId subj_id (Subject subj_id subj_tag)) => IsSubject subj_id (Subject subj_id subj_tag) where
    s_name = _.name <<< unwrap


instance HasTabular (Subject subj_id subj_tag) where
    i_tabular = _.tabular <<< unwrap


init :: forall subj_id subj_tag. subj_id -> String -> Subject subj_id subj_tag
init subj_id name =
    Subject
        { name
        , id : subj_id
        , tags : []
        , tabular : Tabular.empty
        }


derive newtype instance (ReadForeign subj_id,  ReadForeign subj_tag)  => ReadForeign  (Subject subj_id subj_tag)
derive newtype instance (WriteForeign subj_id, WriteForeign subj_tag) => WriteForeign (Subject subj_id subj_tag)