module Test.ParseTree where

import Prelude

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


sampleResultA :: Array (String /\ Array (SampleGroup /\ Array String))
sampleResultA =
    [ "/Some/Directory/" /\
        [ SG [ "." ] /\ [ "i-1" ]
        , SG [ "d1" ] /\ [ "d1-i1", "d1-i2", "d1-i3", "d1-i4" ]
        , SG [ "d2" ] /\ [ "d2-i1", "d2-i2" ]
        , SG [ "d3", "d3-d1" ] /\ [ "d3-d1-i1", "d3-d1-i2", "d3-d1-i3", "d3-d1-i4" ]
        , SG [ "d3", "d3-d2" ] /\ [ "d3-d2-i1", "d3-d2-i2", "d3-d2-i3" ]
        , SG [ "d3", "d3-d3" ] /\ [ "d3-d3-i1", "d3-d3-i2", "d3-d3-i3", "d3-d3-i4", "d3-d3-i5" ]
        , SG [ "d4" ] /\ [ "d4-i1", "d4-i2" ]
        , SG [ "d4", "d4-d1" ] /\ [ "d4-d1-i1", "d4-d1-i2", "d4-d1-i3" ]
        , SG [ "d4", "d4-d2" ] /\ [ "d4-d2-i1" ]
        , SG [ "d5" ] /\ [ "d5-i1", "d5-i2", "d5-i3", "d5-i4" ]
        , SG [ "d5", "d5-d1" ] /\ []
        , SG [ "d5", "d5-d1", "d5-d1-d1" ] /\ [ "d5-d1-d1-i1" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-i1", "d5-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d1" ] /\ [ "d5-d1-d1-d1-d1-d1-i1", "d5-d1-d1-d1-d1-d1-i2" ]
        , SG [ "d5", "d5-d1", "d5-d1-d1", "d5-d1-d1-d1", "d5-d1-d1-d1-d1", "d5-d1-d1-d1-d1-d2" ] /\ [ "d5-d1-d1-d1-d1-d2-i1", "d5-d1-d1-d1-d1-d2-i2" ]
        , SG [ "d5", "d5-d2" ] /\ []
        , SG [ "d5", "d5-d2", "d5-d2-d1" ] /\ [ "d5-d2-d1-i1" ]
        , SG [ "d5", "d5-d2", "d5-d2-d1", "d5-d2-d1-d1" ] /\ [ "d5-d2-d1-d1-i1" ]
        , SG [ "d5", "d5-d3" ] /\ []
        , SG [ "d5", "d5-d3", "d5-d3-d1" ] /\ [ "d5-d3-d1-i1", "d5-d3-d1-i2" ]
        , SG [ "d5", "d5-d3", "d5-d3-d1", "d5-d3-d1-d1" ] /\ [ "d5-d3-d1-d1-i1", "d5-d3-d1-d1-i2" ]
        ]
    ]


type ParseState group =
    { theMap :: Map group (Array String)
    , prevPath :: Array String
    , lastDepth :: Int
    }


initState :: forall group. ParseState group
initState =
    { theMap : Map.empty
    , prevPath : []
    , lastDepth : 0
    }


parseTree :: forall group. Ord group => (Array String -> group) -> String -> String /\ Array (group /\ Array String)
parseTree toGroup sourceStr =
    let
        splitLines = sourceStr # String.split (String.Pattern "\n") <#> String.trim # Array.filter (not <<< String.null)
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
        foldF { theMap, prevPath, lastDepth } line =
            let
                depth = String.length name - String.length line - 1
                checkCP cp = not (CPU.isAlphaNum cp) || CPU.isSpace cp
                name = String.dropWhile checkCP line
                group = toGroup $ Array.take depth prevPath
                newMap = Map.insertWith (flip (<>)) group (Array.singleton name) theMap
                newPrevPath = if depth <= lastDepth then Array.take depth prevPath else prevPath
            in
                { theMap : newMap
                , prevPath : Array.snoc newPrevPath name
                , lastDepth : depth
                }


treeSourceA = """/Some/Directory/
├── d1
│   ├── d1-i1
│   ├── d1-i2
│   ├── d1-i3
│   └── d1-i4
├── d2
│   ├── d2-i1
│   └── d2-i2
├── d3
│   ├── d3-d1
│   │   ├── d3-d1-i1
│   │   ├── d3-d1-i2
│   │   ├── d3-d1-i3
│   │   └── d3-d1-i4
│   ├── d3-d2
│   │   ├── d3-d2-i1
│   │   ├── d3-d2-i2
│   │   └── d3-d2-i3
│   └── d3-d3
│       ├── d3-d3-i1
│       ├── d3-d3-i2
│       ├── d3-d3-i3
│       ├── d3-d3-i4
│       └── d3-d3-i5
├── d4
│   ├── d4-i1
│   ├── d4-i2
│   ├── d4-d1
│   │   ├── d4-d1-i1
│   │   ├── d4-d1-i2
│   │   └── d4-d1-i3
│   └── d4-d2
│       └── d4-d2-i1
├── d5
│   ├── d5-i1
│   ├── d5-d1
│   │   └── d5-d1-d1
│   │       ├── d5-d1-d1-d1
│   │       │   ├── d5-d1-d1-d1-i1
│   │       │   ├── d5-d1-d1-d1-d1
│   │       │   │   ├── d5-d1-d1-d1-d1-d1
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d1-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d1-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-d2
│   │       │   │   │   ├── d5-d1-d1-d1-d1-d2-i1
│   │       │   │   │   └── d5-d1-d1-d1-d1-d2-i2
│   │       │   │   ├── d5-d1-d1-d1-d1-i1
│   │       │   │   └── d5-d1-d1-d1-d1-i2
│   │       │   └── d5-d1-d1-d1-i2
│   │       └── d5-d1-d1-i1
│   ├── d5-i2
│   ├── d5-i3
│   ├── d5-d2
│   │   └── d5-d2-d1
│   │       ├── d5-d2-d1-d1
│   │       │   └── d5-d2-d1-d1-i1
│   │       └── d5-d2-d1-i1
│   ├── d5-d3
│   │   └── d5-d3-d1
│   │       ├── d5-d3-d1-i1
│   │       ├── d5-d3-d1-d1
│   │       │   ├── d5-d3-d1-d1-i1
│   │       │   └── d5-d3-d1-d1-i2
│   │       └── d5-d3-d1-i2
│   └── d5-i4
└── i1
""" :: String


spec :: Spec Unit
spec = do
  describe "parsing tree" $ do
    it "parses tree properly" $
        pure (parseTree SG treeSourceA) `shouldEqual` sampleResultA
