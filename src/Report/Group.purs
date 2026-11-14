module Report.Group where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Foldable (foldl, foldr)
import Data.String (joinWith) as String
import Data.Array (snoc, uncons) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA

import Report.Class
import Report.Decorators.Stats (Stats)
import Report.Decorators.Stats (Stats(..)) as Stats
import Report.Decorators.Tags (RawTag)
import Report.GroupPath (GroupPath(..), PathSegment(..))
import Report.GroupPath as GP
import Report.Modify (class GroupModify, class StatsModify)
import Report.Chain (Chain(..))
import Report.Chain (toArray, toNEArray, fromNEArray) as Chain
import Report.Tabular (Tabular)
import Report.Tabular (empty) as Tabular
import Report.Decorators.Tabular.TabularValue (TabularValue)

import Yoga.JSON (class WriteForeign, class ReadForeign, readImpl)

{- Group -}


newtype Group =
    Group
        { title :: String
        , path :: GroupPath
        , stats :: Stats
        , tabular :: Tabular TabularValue
        }


derive instance Newtype Group _

instance Eq Group where
    eq (Group groupA) (Group groupB) = groupA.path == groupB.path
instance Ord Group where
    compare (Group groupA) (Group groupB) = compare groupA.path groupB.path

derive newtype instance WriteForeign Group
-- `tabular` is optional, so that the groups exported before it was introduced can still be read
instance ReadForeign Group where
    readImpl frgn = do
        grp <- (readImpl frgn :: _ { title :: String, path :: GroupPath, stats :: Stats, tabular :: Maybe (Tabular TabularValue) })
        pure $ Group
            { title : grp.title
            , path : grp.path
            , stats : grp.stats
            , tabular : fromMaybe Tabular.empty grp.tabular
            }


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
        , tabular : Tabular.empty
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


setTabular :: Tabular TabularValue -> Group -> Group
setTabular tabular (Group group) = Group $ group { tabular = tabular }


type FoldStep = { prevPath :: Array String, prevNames :: Array String, groupChain :: NonEmptyArray Group }
-- type FoldStep = NonEmptyArray (Array String /\ String)



mkg :: Array String -> String -> Group
mkg path = mkGroup $ wrap <$> path


mkgw :: Array String -> String -> Stats -> Group
mkgw path = mkGroupWith $ wrap <$> path


-- TODO: same shortcuts for mkGroup / mkGroupWith
cg :: Array String -> String -> Chain Group
cg is l = cgx ((\i -> i /\ i) <$> is) (l /\ l)
cg_ :: String -> Chain Group
cg_ = cg []
cgx :: Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
cgx = quickChain'
cgx_ :: String /\ String -> Chain Group
cgx_ = cgx []


quickChain :: String -> Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
quickChain sep = quickChainP \prev cur -> String.joinWith sep $ Array.snoc prev cur


quickChain' :: Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
quickChain' = quickChainP $ const identity


quickChainP :: (Array String -> String -> String) -> Array ( String /\ String ) -> ( String /\ String ) -> Chain Group
quickChainP makeName pathArr ( lastId /\ lastName ) =
    case Array.uncons pathArr of
        Just { head : firstId /\ firstName, tail } ->
            foldl
                foldF
                (initStep firstId firstName)
                (Array.snoc tail $ ( lastId /\ lastName ))
            # _.groupChain
            # Chain.fromNEArray
        Nothing ->
            Chain.fromNEArray
                $ NEA.singleton
                $ mkGroup [ PathSegment lastId ]
                $ makeName [] lastName
    where
        initStep :: String -> String -> FoldStep
        initStep firstId firstName =
            { prevPath : [ firstId ]
            , prevNames : [ firstName ]
            , groupChain :
                NEA.singleton
                    $ mkGroup [ PathSegment firstId ]
                    $ makeName [] firstName
            }
        foldF :: FoldStep -> String /\ String -> FoldStep
        foldF { prevPath, prevNames, groupChain } (nextId /\ nextName) =
            let
                curPath = Array.snoc prevPath nextId
                curNames = Array.snoc prevNames nextName
                curChain = NEA.snoc groupChain (mkGroup (PathSegment <$> curPath) $ makeName prevNames nextName)
            in
                { prevPath : curPath
                , prevNames : curNames
                , groupChain : curChain
                }

    {-
    foldl
        foldF
        initStep
        pathArr
    # _.groupChain
    # Chain.fromNEArray
    where
        initStep :: FoldStep
        initStep =
            { prevPath : []
            , prevNames : []
            , groupChain :
                NEA.singleton
                    $ mkGroup (PathSegment <$> (Array.snoc (Tuple.fst <$> pathArr) lastId))
                    $ makeName (Tuple.snd <$> pathArr) lastName
            }
        foldF :: FoldStep -> String /\ String -> FoldStep
        foldF (nextId /\ nextName) { prevPath, prevNames, groupChain } =
            let
                curPath = Array.snoc prevPath nextId
                curNames = Array.snoc prevNames nextName
                curChain = NEA.cons (mkGroup (PathSegment <$> curPath) $ makeName prevNames nextName) groupChain
            in
                { prevPath : curPath
                , prevNames : curNames
                , groupChain : curChain
                }
    -}


explode :: Chain Group -> Array (String /\ Array PathSegment)
explode = Chain.toArray >>> map unwrap >>> map \{ title, path } -> title /\ (wrap <$> GP.pathToArray path)


explodeNEA :: Chain Group -> NonEmptyArray (String /\ Array PathSegment)
explodeNEA = Chain.toNEArray >>> map unwrap >>> map \{ title, path } -> title /\ (wrap <$> GP.pathToArray path)


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


instance HasTabular Group where
    i_tabular = unwrap >>> _.tabular


instance GroupModify Group where
    setGroupName name   = unwrap >>> _ { title = name  } >>> wrap

instance StatsModify Group where
    setStats stats = unwrap >>> _ { stats = stats } >>> wrap


instance IsGroupable Group RawTag where
    t_group tag =
        let
            idChain      = tag # unwrap # _.id
            contentChain = tag # unwrap # _.content
            mkGroupLink idx (_ /\ title) =
                Group
                    { stats : Stats.SYetUnknown
                    , title
                    , path : GP.pathFromArray $ NEA.take (idx + 1) idChain
                    , tabular : Tabular.empty
                    }
        in
            Just $ Chain.fromNEArray $ mapWithIndex mkGroupLink $ NEA.zip idChain contentChain


from ::
   forall group
   . IsGroup group
   => HasStats group
   => HasTabular group
   => group
   -> Group
from grp =
    Group
        { title : g_title grp
        , path : g_path grp
        , stats : i_stats grp
        , tabular : i_tabular grp
        }

{-| Concatenate paths of two groups in given order, take stats, title and tabular from the second group. |-}
_cons :: Group -> Group -> Group
_cons (Group { path : pathA }) (Group { path : pathB, stats, title, tabular }) =
    Group { path : pathA <> pathB, stats, title, tabular }


{-| Concatenate paths of two groups in reverse to given order, take stats, title and tabular from the first group. |-}
_snoc :: Group -> Group -> Group
_snoc (Group { path : pathA, stats, title, tabular }) (Group { path : pathB }) =
    Group { path : pathB <> pathA, stats, title, tabular }