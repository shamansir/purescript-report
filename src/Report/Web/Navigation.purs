module Report.Web.Navigation where

import Prelude

import Data.Maybe (Maybe(..))

import Report.GroupPath as GP
import Report.Suffix (Key) as Suffix


type NavigatedTo subj_id = -- TODO: add subj_idect, add tabular key
    { mbSubjectId :: Maybe subj_id
    , mbGroup :: Maybe GP.GroupPath
    , mbItem :: Maybe Int
    , mbSuffix :: Maybe Suffix.Key
    }


init :: forall subj_id. NavigatedTo subj_id
init =
    { mbSubjectId : Nothing
    , mbGroup : Nothing
    , mbItem : Nothing
    , mbSuffix : Nothing
    }


clear :: forall subj_id. NavigatedTo subj_id
clear = init


toGroup :: forall subj_id. subj_id -> GP.GroupPath -> NavigatedTo subj_id
toGroup subj groupPath =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Nothing
    , mbSuffix : Nothing
    }


toItem :: forall subj_id. subj_id ->GP.GroupPath -> Int -> NavigatedTo subj_id
toItem subj groupPath itemIdx =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Nothing
    }


toSuffix :: forall subj_id. subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id
toSuffix subj groupPath itemIdx suffixKey =
    { mbSubjectId : Just subj
    , mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Just suffixKey
    }


atGroup :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> NavigatedTo subj_id -> Boolean
atGroup subj groupPath navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
atItem :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> NavigatedTo subj_id -> Boolean
atItem subj groupPath itemIdx navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
    && navigatedTo.mbItem == Just itemIdx
atSuffix :: forall subj_id. Eq subj_id => subj_id -> GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo subj_id -> Boolean
atSuffix subj groupPath itemIdx suffixKey navigatedTo
    =  navigatedTo.mbSubjectId == Just subj
    && navigatedTo.mbGroup == Just groupPath
    && navigatedTo.mbItem == Just itemIdx
    && navigatedTo.mbSuffix == Just suffixKey