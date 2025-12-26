module Report.Modify where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Prefix (Key) as Prefix
import Report.Suffix (Key) as Suffix
import Report.GroupPath (GroupPath)
import Report (Report)
import Report (withGroup, withItem) as Report


newtype Editable = Editable String


data Where
    = GroupName
    | GroupStat
    | ItemName Int
    | ItemPrefix Int Prefix.Key
    | ItemSuffix Int Suffix.Key
    -- | AddPrefix -- TODO
    -- | AddSuffix -- TODO
    -- | AddItem -- TODO


type Modification subj =
    { subj :: subj
    , path :: GroupPath
    , where_ :: Where
    , newValue :: Editable
    }


class ValueModify a where
    toEditable :: a -> Editable
    fromEditable :: Editable -> Maybe a


modifyAt :: forall subj group item. Ord subj => Modification subj -> Report subj group item -> Maybe (Report subj group item)
modifyAt { subj, where_, newValue, path } report = case where_ of
    GroupName -> do
        Report.withGroup subj path (identity {- TODO -}) report
    GroupStat -> do
        Report.withGroup subj path (identity {- TODO -}) report
    ItemName itemIdx -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report
    ItemPrefix itemIdx pkey -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report
    ItemSuffix itemIdx skey -> do
        Report.withItem subj path itemIdx (identity {- TODO -}) report