module Test.Report where

import Prelude

import Prelude

import Effect.Class (liftEffect)


import Data.Either (either)
import Data.Newtype (unwrap)
import Data.Map (empty) as Map
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

-- import GameLog.StatsTypes as ST

import Foreign as F

import Yoga.JSON as JSON

import Report
import Report as Report
import Report.Builder as RB
import Report.Class (class IsGroup, class HasStats)
import Report.GroupPath (pathFromArray) as ST
import Report.Modifiers.Stats (Stats(..)) as ST

import Yoga.Tree.Extended.Convert (toString) as Tree
import Yoga.Tree.Extended.Convert (Mode(..)) as Mode

import Test.Utils (shouldEqual) as U

-- jsonFilePath = "./data/games/src/manual/Switch/AstralChain.json" :: String


newtype SampleGroup = SG (Array String)


derive newtype instance Eq SampleGroup
derive newtype instance Ord SampleGroup
derive newtype instance Show SampleGroup

instance IsGroup SampleGroup where
    g_title _ = "" -- Not used
    g_path (SG items) = ST.pathFromArray items

instance HasStats SampleGroup where
    i_stats _ = ST.SYetUnknown -- Not used


sampleReport :: Report String SampleGroup String
sampleReport =
    Report.build
        [ "subject1" /\
            [ SG [ "group-1" ] /\ [ "group-1-item-1", "group-1-item-2" ]
            , SG [ "group-1", "group-1-1" ] /\ [ "group-1-1-item-1", "group-1-1-item-2" ]
            , SG [ "group-1", "group-1-1", "group-1-1-1" ] /\ [ "group-1-1-1-item-1", "group-1-1-1-item-2" ]
            , SG [ "group-1", "group-1-2" ] /\ [ "group-1-2-item-1", "group-1-2-item-2" ]
            , SG [ "group-1", "group-1-3" ] /\ [ "group-1-3-item-1", "group-1-3-item-2" ]
            , SG [ "group-1", "group-1-4" ] /\ [ ]
            , SG [ "group-1", "group-1-4", "group-1-4-1" ] /\ [ "group-1-4-1-item-1", "group-1-4-1-item-2" ]
            , SG [ "group-1", "group-1-4", "group-1-4-2" ] /\ [ "group-1-4-2-item-1", "group-1-4-2-item-2" ]
            , SG [ "group-1", "group-1-4", "group-1-4-2", "group-1-4-2-1" ] /\ [ "group-1-4-2-1-item-1" ]
            , SG [ "group-1", "group-1-4", "group-1-4-3" ] /\ [ ]
            , SG [ "group-1", "group-1-4", "group-1-4-4" ] /\ [ "group-1-4-4-item-1" ]
            , SG [ "group-2" ] /\ [ "group-2-item-1", "group-1-item-2" ]
            , SG [ "group-2", "group-2-1" ] /\ [ "group-2-1-item-1", "group-2-1-item-2" ]
            , SG [ "group-2", "group-2-1", "group-2-1-1" ] /\ [ "group-2-1-1-item-1", "group-2-1-1-item-2" ]
            , SG [ "group-1", "group-2-2" ] /\ [ ]
            ]
        ]

spec :: Spec Unit
spec = do
  describe "converting to tree" $ do
    it "properly converts storage to tree" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree sampleReport)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        """*
┊S: "subject1"
┊┄G: ["group-1"]
┊┄┄I: "group-1-item-1"
┊┄┄I: "group-1-item-2"
┊┄┄G: ["group-1","group-1-1"]
┊┄┄┄I: "group-1-1-item-1"
┊┄┄┄I: "group-1-1-item-2"
┊┄┄┄G: ["group-1","group-1-1","group-1-1-1"]
┊┄┄┄┄I: "group-1-1-1-item-1"
┊┄┄┄┄I: "group-1-1-1-item-2"
┊┄┄G: ["group-1","group-1-2"]
┊┄┄┄I: "group-1-2-item-1"
┊┄┄┄I: "group-1-2-item-2"
┊┄┄G: ["group-1","group-1-3"]
┊┄┄┄I: "group-1-3-item-1"
┊┄┄┄I: "group-1-3-item-2"
┊┄┄G: ["group-1","group-1-4"]
┊┄┄┄G: ["group-1","group-1-4","group-1-4-1"]
┊┄┄┄┄I: "group-1-4-1-item-1"
┊┄┄┄┄I: "group-1-4-1-item-2"
┊┄┄┄G: ["group-1","group-1-4","group-1-4-2"]
┊┄┄┄┄I: "group-1-4-2-item-1"
┊┄┄┄┄I: "group-1-4-2-item-2"
┊┄┄┄┄G: ["group-1","group-1-4","group-1-4-2","group-1-4-2-1"]
┊┄┄┄┄┄I: "group-1-4-2-1-item-1"
┊┄┄┄G: ["group-1","group-1-4","group-1-4-3"]
┊┄┄┄G: ["group-1","group-1-4","group-1-4-4"]
┊┄┄┄┄I: "group-1-4-4-item-1"
┊┄┄G: ["group-1","group-2-2"]
┊┄G: ["group-2"]
┊┄┄I: "group-2-item-1"
┊┄┄I: "group-1-item-2"
┊┄┄G: ["group-2","group-2-1"]
┊┄┄┄I: "group-2-1-item-1"
┊┄┄┄I: "group-2-1-item-2"
┊┄┄┄G: ["group-2","group-2-1","group-2-1-1"]
┊┄┄┄┄I: "group-2-1-1-item-1"
┊┄┄┄┄I: "group-2-1-1-item-2""""