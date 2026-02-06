module Report.Group where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.String (joinWith) as String
import Data.Array (snoc) as Array

import Report.Class
import Report.Modifiers.Stats (Stats)
import Report.Modifiers.Stats (Stats(..)) as Stats
import Report.GroupPath (GroupPath(..), PathSegment(..))
import Report.Modify (class GroupModify, class StatsModify)
import Report.Chain (Chain(..))

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



mkGroup :: Array PathSegment -> String -> Group
mkGroup path title =
    mkGroupWith path title Stats.SYetUnknown


mkGroupWith :: Array PathSegment -> String -> Stats -> Group
mkGroupWith path title stats =
    Group
        { title
        , stats
        , path : GroupPath path
        }


rootGroup :: Group
rootGroup = rootGroupWith Stats.SYetUnknown


rootGroupWith :: Stats -> Group
rootGroupWith stats =
    mkGroupWith
        [ PathSegment "root" ]
        "All"
        stats


unknownGroup :: Group
unknownGroup =
    mkGroupWith
        [ PathSegment "unknown" ]
        "Unknown"
        Stats.SYetUnknown


isGroupAt :: GroupPath -> Group -> Boolean
isGroupAt path (Group group) = group.path == path


pathOf :: Group -> GroupPath
pathOf (Group group) = group.path


setStats :: Stats -> Group -> Group
setStats stats (Group group) = Group $ group { stats = stats }


type FoldStep = { prevPath :: Array String, prevNames :: Array String, groupChain :: Chain Group }


quickChain :: String -> Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
quickChain sep = quickChain_ \prev cur -> String.joinWith sep $ Array.snoc prev cur


quickChain_ :: (Array String -> String -> String) -> Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
quickChain_ makeName pathArr ( lastId /\ lastName ) =
    foldl
        foldF
        initStep
        pathArr
    # _.groupChain
    where
        initStep :: FoldStep
        initStep =
            { prevPath : []
            , prevNames : []
            , groupChain :
                End
                    $ mkGroup (PathSegment <$> (Array.snoc (Tuple.fst <$> pathArr) lastId))
                    $ makeName (Tuple.snd <$> pathArr) lastName
            }
        foldF :: FoldStep -> String /\ String -> FoldStep
        foldF { prevPath, prevNames, groupChain } (nextId /\ nextName) =
            let
                curPath = Array.snoc prevPath nextId
                curNames = Array.snoc prevNames nextName
                curChain = More (mkGroup (PathSegment <$> curPath) $ makeName prevNames nextName) groupChain
            in
                { prevPath : curPath
                , prevNames : curNames
                , groupChain : curChain
                }


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