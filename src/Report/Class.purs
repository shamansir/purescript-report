module Report.Class where

import Prelude

import Data.Maybe (Maybe)

import Report.Suffix (Suffixes)
import Report.Prefix (Prefixes)
import Report.Tabular (Tabular)
import Report.Modifiers.Tabular.TabularValue (TabularValue)
import Report.Modifiers.Stats (Stats) as S
import Report.GroupPath (GroupPath) as S


class IsItem t a where
    i_name :: a -> String
    i_prefixes :: a -> Prefixes
    i_suffixes :: a -> Suffixes t
    i_mbTitle :: a -> Maybe String
    i_locked :: a -> Boolean
    i_tabular :: a -> Tabular TabularValue


class IsGroup a where
    g_title :: a -> String
    g_path :: a -> S.GroupPath
    g_stats :: a -> S.Stats


class IsSubjectId i a where
    s_id :: a -> i


class IsSubjectId i a <= IsSubject i t a where
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
    tagContent :: t -> String
    decodeTag :: String -> Maybe t
    allTags :: Array t