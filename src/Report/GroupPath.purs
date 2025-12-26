module Report.GroupPath where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (joinWith) as String
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Array (length, index) as Array

import Yoga.JSON (class ReadForeign, class WriteForeign)


{- GroupPath -}


newtype PathSegment = PathSegment String
derive newtype instance ReadForeign PathSegment
derive newtype instance WriteForeign PathSegment

newtype GroupPath = GroupPath (Array PathSegment)


derive instance Newtype PathSegment _
derive instance Newtype GroupPath _
instance Show GroupPath where show = unwrap >>> map unwrap >>> String.joinWith "::"
derive newtype instance ReadForeign GroupPath
derive newtype instance WriteForeign GroupPath


instance Eq GroupPath where
    eq pathA pathB = (unwrap <$> unwrap pathA) == (unwrap <$> unwrap pathB)
instance Ord GroupPath where
    compare pathA pathB = compare (unwrap <$> unwrap pathA) (unwrap <$> unwrap pathB)


howDeep :: GroupPath -> Int
howDeep = unwrap >>> Array.length


pathToArray :: GroupPath -> Array String
pathToArray = unwrap >>> map unwrap


pathFromArray :: Array String -> GroupPath
pathFromArray = map wrap >>> wrap


startsWithNotEq :: GroupPath -> GroupPath -> Boolean
startsWithNotEq possibleStart sample =
    (howDeep possibleStart > 0) &&
    (howDeep sample > 0) &&
    (howDeep possibleStart < howDeep sample) &&
    ( let
        sampleArr = pathToArray sample
        possibleStartArr = pathToArray possibleStart
    in
        foldlWithIndex
            (\idx prev val -> prev && (Array.index sampleArr idx == Just val))
            true
            possibleStartArr
    )


{-
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
-}