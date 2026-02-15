module Test.ParseTree where

import Prelude

import Debug as Debug

import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.String as String
import Data.Foldable (foldl)
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.String.CodePoints as CP
import Data.CodePoint.Unicode as CPU

import Test.Spec (Spec, it, itOnly, describe, pending')
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Report (Report)
import Report as Report
import Report.Class
import Report.GroupPath as GP
import Report.Modifiers.Stats as ST

newtype SampleGroup = SG (Array String)


derive newtype instance Eq SampleGroup
derive newtype instance Ord SampleGroup
derive newtype instance Show SampleGroup

instance IsGroup SampleGroup where
    g_title _ = "" -- Not used
    g_path (SG items) = GP.pathFromArray items

instance HasStats SampleGroup where
    i_stats _ = ST.SYetUnknown -- Not used



type ParseState group =
    { theMap :: Map group (Array String)
    , location :: Array String
    , lastDepth :: Int
    }


initState :: forall group. ParseState group
initState =
    { theMap : Map.empty
    , location : [ ]
    , lastDepth : 0
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
        foldF { theMap, location, lastDepth } line =
            let
                -- _ = Debug.spy "line" line
                _ = Debug.spy "____" "____"
                checkCP cp = not (CPU.isAlphaNum cp) || CPU.isSpace cp
                name = Debug.spy "name" $ String.dropWhile checkCP line
                _ = Debug.spy "lastDepth" lastDepth
                depth = Debug.spy "depth" $ ((String.length line - String.length name) `div` 4)
                prevLocation = Debug.spy "prevLocation" $ location
                curLocation = Debug.spy "curLocation" $ Array.take (depth - 1) location
                nextLocation = Debug.spy "nextLocation" $
                    if (depth == 0) then [ name ]
                    else if (depth > Array.length curLocation) then Array.snoc curLocation name
                    else curLocation
                    -- else prevLocation
                -- nextLocation = Debug.spy "nextLocation" $
                --     if (lastDepth == 0) then [ name ]
                --     else if (depth /= lastDepth) then Array.snoc curLocation name
                --     else if (depth <  lastDepth) then curLocation
                --     else prevLocation
                group = toGroup $ if curLocation == [] then [ "." ] else curLocation
                newMap = Map.insertWith ((<>)) group (Array.singleton name) theMap
            in
                { theMap : newMap
                , location : nextLocation
                , lastDepth : depth
                }


filterDirectories :: Array String -> Array String
filterDirectories = Array.filter \s -> (s # String.drop (String.length s - 2) # String.take 1) /= "d"


treeSourceA = """/Some/Directory/
└── d1
    ├── d1-i1
    ├── d1-i2
    ├── d1-i3
    └── d1-i4
""" :: String


treeSourceB = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
└── d3
    ├── d3-d1
    │   ├── d3-d1-i1
    │   ├── d3-d1-i2
    │   ├── d3-d1-i3
    │   └── d3-d1-i4
    ├── d3-d2
    │   ├── d3-d2-i1
    │   ├── d3-d2-i2
    │   └── d3-d2-i3
    └── d3-d3
        ├── d3-d3-i1
        ├── d3-d3-i2
        ├── d3-d3-i3
        ├── d3-d3-i4
        └── d3-d3-i5
""" :: String



treeSourceC = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
├── d3
│   ├── d3-d1
│   │   ├── d3-d1-i1
│   │   ├── d3-d1-i2
│   │   ├── d3-d1-i3
│   │   └── d3-d1-i4
│   ├── d3-d2
│   │   ├── d3-d2-i1
│   │   ├── d3-d2-i2
│   │   └── d3-d2-i3
│   └── d3-d3
│       ├── d3-d3-i1
│       ├── d3-d3-i2
│       ├── d3-d3-i3
│       ├── d3-d3-i4
│       └── d3-d3-i5
└── d4
    ├── d4-i1
    ├── d4-i2
    ├── d4-d1
    │   ├── d4-d1-i1
    │   ├── d4-d1-i2
    │   └── d4-d1-i3
    └── d4-d2
        └── d4-d2-i1
""" :: String


treeSourceD = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
├── d3
│   ├── d3-d1
│   │   ├── d3-d1-i1
│   │   ├── d3-d1-i2
│   │   ├── d3-d1-i3
│   │   └── d3-d1-i4
│   ├── d3-d2
│   │   ├── d3-d2-i1
│   │   ├── d3-d2-i2
│   │   └── d3-d2-i3
│   └── d3-d3
│       ├── d3-d3-i1
│       ├── d3-d3-i2
│       ├── d3-d3-i3
│       ├── d3-d3-i4
│       └── d3-d3-i5
├── d4
│   ├── d4-i1
│   ├── d4-i2
│   ├── d4-d1
│   │   ├── d4-d1-i1
│   │   ├── d4-d1-i2
│   │   └── d4-d1-i3
│   └── d4-d2
│       └── d4-d2-i1
├── d5
│   ├── d5-i1
│   ├── d5-d1
│   │   └── d5-d1-d1
│   │       ├── d5-d1-d1-d1
│   │       │   ├── d5-d1-d1-d1-i1
│   │       │   ├── d5-d1-d1-d1-d1
│   │       │   │   ├── d5-d1-d1-d1-d1-d1
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d1-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d1-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-d2
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d2-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d2-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-i1
│   │       │   │   └── d5-d1-d1-d1-d1-i2
│   │       │   └── d5-d1-d1-d1-i2
│   │       └── d5-d1-d1-i1
│   ├── d5-i2
│   ├── d5-i3
│   ├── d5-d2
│   │   └── d5-d2-d1
│   │       ├── d5-d2-d1-d1
│   │       │   └── d5-d2-d1-d1-i1
│   │       └── d5-d2-d1-i1
│   ├── d5-d3
│   │   └── d5-d3-d1
│   │       ├── d5-d3-d1-i1
│   │       ├── d5-d3-d1-d1
│   │       │   ├── d5-d3-d1-d1-i1
│   │       │   └── d5-d3-d1-d1-i2
│   │       └── d5-d3-d1-i2
│   └── d5-i4
└── i1
""" :: String


sampleResultA :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultA =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        ]
    ]


sampleResultB :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultB =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        ]
    ]



sampleResultC :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultC =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        , SG [ "d4" ] /\ [ "d4-i1", "d4-i2" ]
        , SG [ "d4", "d4-d1" ] /\ [ "d4-d1-i1", "d4-d1-i2", "d4-d1-i3" ]
        , SG [ "d4", "d4-d2" ] /\ [ "d4-d2-i1" ]
        ]
    ]



sampleResultD :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultD =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ "i1" ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3" ] /\ [ ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        , SG [ "d4" ] /\ [ "d4-i1", "d4-i2" ]
        , SG [ "d4", "d4-d1" ] /\ [ "d4-d1-i1", "d4-d1-i2", "d4-d1-i3" ]
        , SG [ "d4", "d4-d2" ] /\ [ "d4-d2-i1" ]
        , SG [ "d5" ] /\ [ "d5-i1", "d5-i2", "d5-i3", "d5-i4" ]
        , SG [ "d5", "d5-d1" ] /\ [ ]
        , SG [ "d5", "d5-d1", "d5-d1-d1" ] /\ [ "d5-d1-d1-i1" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-i1", "d5-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d2" ] /\ [ "d5-d1-d1-d1-d1-d2-i1", "d5-d1-d1-d1-d1-d2-i2" ]
        , SG [ "d5", "d5-d2" ] /\ [ ]
        , SG [ "d5", "d5-d2", "d5-d2-d1" ] /\ [ "d5-d2-d1-i1" ]
        , SG [ "d5", "d5-d2", "d5-d2-d1", "d5-d2-d1-d1" ] /\ [ "d5-d2-d1-d1-i1" ]
        , SG [ "d5", "d5-d3" ] /\ [ ]
        , SG [ "d5", "d5-d3", "d5-d3-d1" ] /\ [ "d5-d3-d1-i1", "d5-d3-d1-i2" ]
        , SG [ "d5", "d5-d3", "d5-d3-d1", "d5-d3-d1-d1" ] /\ [ "d5-d3-d1-d1-i1", "d5-d3-d1-d1-i2" ]
        ]
    ]


makeSample :: String -> Array (String /\ Array (SampleGroup /\ Array String))
makeSample = parseTree SG >>> map (map $ map filterDirectories) >>> pure


spec :: Spec Unit
spec = do
  describe "parsing tree" $ do
    it "parses tree properly: A" $
        makeSample treeSourceA `shouldEqual` sampleResultA
    it "parses tree properly: B" $
        makeSample treeSourceB `shouldEqual` sampleResultB
    it "parses tree properly: C" $
       makeSample treeSourceC `shouldEqual` sampleResultC
    it "parses tree properly: D" $
       makeSample treeSourceD `shouldEqual` sampleResultD
