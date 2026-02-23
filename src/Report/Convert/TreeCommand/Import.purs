module Report.Convert.TreeCommand.Import where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodePoints as CP
import Data.CodePoint.Unicode as CPU

-- import Report (Report)
-- import Report as Report
-- import Report.Class
-- import Report.GroupPath as GP
-- import Report.Decorators.Stats as ST


type ParseState group =
    { theMap :: Map group (Array String)
    , lastSameLevelPath :: Array String
    }


initState :: forall group. ParseState group
initState =
    { theMap : Map.empty
    , lastSameLevelPath : [ ]
    }


parseTree :: forall group. Ord group => (Array String -> group) -> String -> String /\ Array (group /\ Array String)
parseTree toGroup sourceStr =
    let
        splitLines = sourceStr # String.split (String.Pattern "\n") # Array.filter (not <<< String.null)
        mbRootDir = Array.take 1 splitLines # Array.head
    in
        case mbRootDir of
            Just rootDir ->
                rootDir /\
                parseLines (Array.drop 1 splitLines)
            Nothing ->
                "" /\
                []

    where
        parseLines :: Array String -> Array (group /\ Array String)
        parseLines = foldl foldF initState >>> _.theMap >>> Map.toUnfoldable
        foldF :: ParseState group -> String -> ParseState group
        foldF { theMap, lastSameLevelPath } line =
            let
                checkCP cp = not (CPU.isAlphaNum cp || (CP.singleton cp == "_")) || CPU.isSpace cp
                name = String.dropWhile checkCP line
                depth = (String.length line - String.length name) `div` 4
                curLocation = Array.take (depth - 1) lastSameLevelPath
                nextSameLevelPath =
                    if (depth == 0) then [ name ]
                    else if (depth > Array.length curLocation) then Array.snoc curLocation name
                    else curLocation
                group = toGroup $ if curLocation == [] then [ "." ] else curLocation
                newMap = Map.insertWith ((<>)) group (Array.singleton name) theMap
            in
                { theMap : newMap
                , lastSameLevelPath : nextSameLevelPath
                }


