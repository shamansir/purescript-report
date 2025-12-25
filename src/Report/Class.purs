module Report.Class where

import Prelude

import Data.Maybe (Maybe)

import Report.Core as CT
import Report.Progress (Progress) as S
import Report.Stats (Stats) as S
import Report.Group (GroupPath) as S


class IsItem t a where
    i_name :: a -> String
    i_progress :: a -> S.Progress
    i_mbTitle :: a -> Maybe String -- FIXME: get rid of
    i_mbDescription :: a -> Maybe String
    i_mbEarnedAt :: a -> Maybe CT.SDate
    i_locked :: a -> Boolean
    i_mbReference :: a -> Maybe S.GroupPath
    i_tags :: a -> Array t


class IsGroup a where
    g_title :: a -> String
    g_path :: a -> S.GroupPath
    g_stats :: a -> S.Stats


class IsSubject i t a where
    s_id :: a -> i
    s_name :: a -> String
    s_tags :: a -> Array t
    s_stats :: a -> S.Stats


type TagColors =
    { text :: String
    , background :: String
    , border :: String
    }

class Eq t <= IsTag t where
    tagColors :: t -> TagColors
    tagContent   :: t -> String
    allTags :: Array t