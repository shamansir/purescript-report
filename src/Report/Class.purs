module Report.Class where

import Prelude

import Data.Maybe (Maybe(..))

import Report.Suffix (Suffixes)
import Report.Prefix (Prefixes)
import Report.Tabular (Tabular)
import Report.Decorators.Tabular.TabularValue (TabularValue)
import Report.Decorators.Stats (Stats) as S
import Report.GroupPath (GroupPath) as S
import Report.Chain (Chain(..))
import Report.Decorator (Decorators)


class HasDecorators t a where
    i_decorators :: a -> Decorators t


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


-- newtype GChain g = GChain (Chain g)


class IsGroup g <= IsGroupable g t where
    t_group :: t -> Maybe (Chain g)


-- used for tags, so when we sort items by a tag, we can find the "same kind" tag on each item, i.e. rating or platform
-- and if they are the same, sort by their `Ord` instance
class IsSortable t where
    sameKind :: t -> t -> Boolean


class Eq t <= IsTag t where
    tagColors :: t -> TagColors
    tagContent :: t -> Chain String
    decodeTag :: String -> Maybe t
    allTags :: Array t


instance IsTag Unit where
    tagColors _ = { text: "#000000", background: "#FFFFFF", border: "#CCCCCC" }
    tagContent _ = End ""
    decodeTag _ = Just unit
    allTags = [unit]