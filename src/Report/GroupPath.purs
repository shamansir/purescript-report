module Report.GroupPath where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.String (split, joinWith, Pattern(..)) as String
import Data.Array (length, index) as Array
import Data.FoldableWithIndex (foldlWithIndex)
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


encode :: GroupPath -> String
encode gp = String.joinWith "::" (pathToArray gp)


decode :: String -> Maybe GroupPath
decode str = case String.split (String.Pattern "::") str of
    [] -> Nothing
    arr -> Just $ pathFromArray arr