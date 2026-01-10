module Report.Group where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)

import Report.Class
import Report.Modifiers.Stats (Stats)
import Report.GroupPath (GroupPath(..), PathSegment(..))
import Report.Modify (class GroupModify, class StatsModify)

import Yoga.JSON (class WriteForeign)

{- Group -}


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

derive newtype instance WriteForeign Group


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


instance HasStats Group where
    i_stats = unwrap >>> _.stats


instance GroupModify Group where
    setGroupName name   = unwrap >>> _ { title = name  } >>> wrap

instance StatsModify Group where
    setStats stats = unwrap >>> _ { stats = stats } >>> wrap



from ::
   forall group
   . IsGroup group
   => HasStats group
   => group
   -> Group
from grp =
    Group
        { title : g_title grp
        , path : g_path grp
        , stats : i_stats grp
        }