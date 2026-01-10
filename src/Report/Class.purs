module Report.Class where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Suffix (Suffixes)
import Report.Prefix (Prefixes)
import Report.Tabular (Tabular)
import Report.Modifiers.Tabular.TabularValue (TabularValue)
import Report.Modifiers.Stats (Stats) as S
import Report.GroupPath (GroupPath) as S


class HasPrefixes a where
    i_prefixes :: a -> Prefixes


class HasSuffixes t a where
    i_suffixes :: a -> Suffixes t


class (HasPrefixes a, HasSuffixes t a) <= HasModifiers t a


class HasTabular a where
    i_tabular :: a -> Tabular TabularValue


class HasTags t a where
    i_tags :: a -> Array t


class HasStats a where
    i_stats :: a -> S.Stats


class IsItem a where
    i_name :: a -> String
    i_mbTitle :: a -> Maybe String
    i_locked :: a -> Boolean


class IsGroup a where
    g_title :: a -> String
    g_path :: a -> S.GroupPath


class IsSubjectId i a where
    s_id :: a -> i
    s_unique :: i -> String


class IsSubjectId i a <= IsSubject i a where
    s_name :: a -> String


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


instance IsTag Unit where
    tagColors _ = { text: "#000000", background: "#FFFFFF", border: "#CCCCCC" }
    tagContent _ = ""
    decodeTag _ = Just unit
    allTags = [unit]