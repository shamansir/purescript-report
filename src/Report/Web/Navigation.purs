module Report.Web.Navigation where

import Prelude

import Data.Maybe (Maybe(..))

import Report.GroupPath as GP
import Report.Suffix (Key) as Suffix


type NavigatedTo = -- TODO: add subject, add tabular key
    { mbGroup :: Maybe GP.GroupPath
    , mbItem :: Maybe Int
    , mbSuffix :: Maybe Suffix.Key
    }


init :: NavigatedTo
init =
    { mbGroup : Nothing
    , mbItem : Nothing
    , mbSuffix : Nothing
    }


clear :: NavigatedTo
clear = init


toGroup :: GP.GroupPath -> NavigatedTo
toGroup groupPath =
    { mbGroup : Just groupPath
    , mbItem : Nothing
    , mbSuffix : Nothing
    }


toItem :: GP.GroupPath -> Int -> NavigatedTo
toItem groupPath itemIdx =
    { mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Nothing
    }


toSuffix :: GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo
toSuffix groupPath itemIdx suffixKey =
    { mbGroup : Just groupPath
    , mbItem : Just itemIdx
    , mbSuffix : Just suffixKey
    }


atGroup :: GP.GroupPath -> NavigatedTo -> Boolean
atGroup groupPath navigatedTo
    =  navigatedTo.mbGroup == Just groupPath
atItem :: GP.GroupPath -> Int -> NavigatedTo -> Boolean
atItem groupPath itemIdx navigatedTo
    =  navigatedTo.mbGroup == Just groupPath
    && navigatedTo.mbItem == Just itemIdx
atSuffix :: GP.GroupPath -> Int -> Suffix.Key -> NavigatedTo -> Boolean
atSuffix groupPath itemIdx suffixKey navigatedTo
    =  navigatedTo.mbGroup == Just groupPath
    && navigatedTo.mbItem == Just itemIdx
    && navigatedTo.mbSuffix == Just suffixKey