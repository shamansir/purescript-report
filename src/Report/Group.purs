module Report.Group where

import Prelude

import Data.Newtype (class Newtype, unwrap)

import Report.Class (class IsGroup)
import Report.Stats (Stats(..))
import Report.GroupPath (GroupPath(..), PathSegment(..))

{- Achievement Group -}


newtype Group =
    Group
        { title :: String
        , path :: GroupPath
        , stats :: Stats
        }

derive instance Newtype Group _

instance Eq Group where
    eq (Group groupA) (Group groupB) = groupA.path == groupB.path
instance Ord Group where
    compare (Group groupA) (Group groupB) = compare groupA.path groupB.path


instance Show Group where
    show (Group { title, path }) =
        title <> " (" <> show path <> ")"


rootGroup :: Stats -> Group
rootGroup stats =
    Group
        { title : "All"
        , stats
        , path : GroupPath [ PathSegment "root" ]
        }


isGroupAt :: GroupPath -> Group -> Boolean
isGroupAt path (Group group) = group.path == path


pathOf :: Group -> GroupPath
pathOf (Group group) = group.path


setStats :: Stats -> Group -> Group
setStats stats (Group group) = Group $ group { stats = stats }


{-
group :: String -> Array Achievement -> Group
group name children =
    ach_ name None # with (_ { items = children })
-}


instance IsGroup Group where
    g_title = unwrap >>> _.title
    g_path = unwrap >>> _.path
    g_stats = unwrap >>> _.stats