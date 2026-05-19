module Test.Report where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\), type (/\))

import Test.Spec (Spec, it, itOnly, describe, pending')
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)

-- import GameLog.StatsTypes as ST

import Report (Report)
import Report as Report
import Report.Chain as C
import Report.Builder as RB
import Report.Class (class HasStats, class HasTags, class IsGroup, class IsGroupable, class IsSortable, class Same)
import Report.Convert.Generic (class ToImport, class ToExport)
import Report.GroupPath (pathFromArray) as GP
import Report.Decorators.Stats (Stats(..)) as ST

import Yoga.Tree.Extended.Convert (toString) as Tree
import Yoga.Tree.Extended.Convert (Mode(..)) as Mode

import Test.Utils (shouldEqual) as U

import Test.Samples.ArtistsReport (Subject(..), Artist(..), ArtistTag, ArtistTagKind(..), MyGroup(..), artistsReport)

-- jsonFilePath = "./data/games/src/manual/Switch/AstralChain.json" :: String


newtype SampleGroup = SG (Array String)


derive newtype instance Eq SampleGroup
derive newtype instance Ord SampleGroup
derive newtype instance Show SampleGroup

instance IsGroup SampleGroup where
    g_title _ = "" -- Not used
    g_path (SG items) = GP.pathFromArray items

instance HasStats SampleGroup where
    i_stats _ = ST.SYetUnknown -- Not used


sampleReportA :: Report String SampleGroup String
sampleReportA =
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


newtype BoolTag = BoolTag Boolean
derive instance Newtype BoolTag _


newtype TagTest1Item = TT1I String
derive instance Newtype TagTest1Item _
derive newtype instance Show TagTest1Item
derive newtype instance Eq TagTest1Item

instance HasTags BoolTag TagTest1Item where
    i_tags = unwrap >>> case _ of
        "item1" -> [ true ]
        "item2" -> [ false ]
        "item3" -> [ true ]
        "item4" -> [ false ]
        _ -> []
        >>> map BoolTag


data BoolTagKind
    = Truthful
    | Falseful


derive instance Eq BoolTagKind


instance IsSortable BoolTagKind BoolTag where
    kindOf (BoolTag v) = if v then Truthful else Falseful



instance Same BoolTagKind where
    same = const $ const true



instance IsGroupable MyGroup BoolTag where
    t_group = unwrap >>> case _ of
        true -> Just $ C.End $ G [ "true" ]
        false -> Just $ C.End $ G [ "false" ]


spec :: Spec Unit
spec = do
  describe "unfolding" $ do
    it "unfolds properly" $
        let
            reportSrc =
                [ "subj" /\
                    [ G [ "Analogue" ] /\ (A <$> [ ])
                    , G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
                    , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen" ])
                    , G [ "Electronic" ] /\ (A <$> [ ])
                    , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
                    , G [ "Test", "Test A", "Test B" ] /\ (A <$> [ "I1", "I2" ])
                    , G [ "Test" ] /\ (A <$> [ "I3" ])
                    ]
                ]
            report = RB.build reportSrc # Report.fromBuilder
        in (Report.unfold report) `shouldEqual` reportSrc

    it "unfolds properly, p.2" $
        let
            reportSrc =
                [ "subj" /\
                    [ G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
                    , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen" ])
                    , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
                    ]
                ]
            report = RB.build reportSrc # Report.fromBuilder
        in (Report.unfold report) `shouldEqual` reportSrc

    it "unfolds properly (group-chains)" $
        let
            reportSrc =
                [ "subj" /\
                    [ G [ "Analogue" ] /\ (A <$> [ ])
                    , G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
                    , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen" ])
                    , G [ "Electronic" ] /\ (A <$> [ ])
                    , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
                    , G [ "Test", "Test A", "Test B" ] /\ (A <$> [ "I1", "I2" ])
                    , G [ "Test" ] /\ (A <$> [ "I3" ])
                    ]
                ]
            report = RB.buildG reportSrc # Report.fromBuilder
        in (Report.unfold report) `shouldEqual` reportSrc

    it "unfolds properly (group-chains), p.2" $
        let
            reportSrc =
                [ "subj" /\
                    [ G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
                    , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen" ])
                    , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
                    ]
                ]
            report = RB.buildG reportSrc # Report.fromBuilder
        in (Report.unfold report) `shouldEqual` reportSrc

    it "unfolds properly (group-chains to chains)" $
        let
            reportSrc =
                [ "subj" /\
                    [ G [ "Analogue" ] /\ (A <$> [ ])
                    , G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
                    , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen" ])
                    , G [ "Electronic" ] /\ (A <$> [ ])
                    , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
                    , G [ "Test" ] /\ (A <$> [ ])
                    , G [ "Test", "Test A" ] /\ (A <$> [ ]) -- FIXME: we have to have an empty group here or else grouping mechanics won't work (no path known for "Test A" since it's not in the set)
                    , G [ "Test", "Test A", "Test B" ] /\ (A <$> [ "I1", "I2" ])
                    , G [ "Test" ] /\ (A <$> [ "I3" ])
                    ]
                ]

            reportTrg =
                [ "subj" /\
                    [ (C.End $ G [ "Analogue" ]) /\ (A <$> [ ])
                    , (C.More (G [ "Analogue" ]) $ C.End $ G [ "Analogue", "Rock" ] ) /\ (A <$> [ "Queen", "Rammstein" ])
                    , (C.More (G [ "Analogue" ]) $ C.More (G [ "Analogue", "Rock" ]) $ C.End $ G [ "Analogue", "Rock", "Pop Rock" ] ) /\ (A <$> [ "Queen" ])
                    , (C.End $ G [ "Electronic" ]) /\ (A <$> [ ])
                    , (C.More (G [ "Electronic" ]) $ C.End $  G [ "Electronic", "Industrial" ] ) /\ (A <$> [ "NIN", "Rammstein" ])
                    , (C.End $ G [ "Test" ]) /\ (A <$> [ ])
                    , (C.More (G [ "Test" ]) $ C.End $ G [ "Test", "Test A" ]) /\ (A <$> [ ])
                    , (C.More (G [ "Test" ]) $ C.More (G [ "Test", "Test A" ]) $ C.End $ G [ "Test", "Test A", "Test B" ] ) /\ (A <$> [ "I1", "I2" ])
                    , (C.End $ G [ "Test" ]) /\ (A <$> [ "I3" ])
                    ]
                ]
            -- [(Tuple "subj" [(Tuple ["Analogue"] []),(Tuple ["Analogue"]::["Analogue","Rock"] ["Queen","Rammstein"]),(Tuple ["Analogue"]::["Analogue","Rock"]::["Analogue","Rock","Pop Rock"] ["Queen"]),(Tuple ["Electronic"] []),(Tuple ["Electronic"]::["Electronic","Industrial"] ["NIN","Rammstein"]),(Tuple ["Test"]::["Test","Test A","Test B"] ["I1","I2"]),(Tuple ["Test"] ["I3"])])]
            -- [(Tuple "subj" [(Tuple ["Analogue"] []),(Tuple ["Analogue"]::["Analogue","Rock"] ["Queen","Rammstein"]),(Tuple ["Analogue"]::["Analogue","Rock"]::["Analogue","Rock","Pop Rock"] ["Queen"]),(Tuple ["Electronic"] []),(Tuple ["Electronic"]::["Electronic","Industrial"] ["NIN","Rammstein"]),(Tuple ["Test"]::["Test","Test A"]::["Test","Test A","Test B"] ["I1","I2"]),(Tuple ["Test"] ["I3"])])]
            report = RB.buildG reportSrc # Report.fromBuilder
        in (Report.unfoldC report) `shouldEqual` reportTrg


  describe "grouping by tag" $ do
    it "re-groups report by tag" $ do
        let
            report =
                RB.buildG
                    [ "subj" /\ [ G [ "root" ] /\ (TT1I <$> [ "item1", "item2", "item3", "item4" ]) ] ]
                    # Report.fromBuilder
        (report # Report.groupItemsByKind @BoolTag Truthful # Report.unfold)
        `shouldEqual`
            [ "subj" /\
                [ G [ "false" ] /\ (TT1I <$> [ "item2", "item4" ])
                , G [ "true"  ] /\ (TT1I <$> [ "item1", "item3" ])
                ]
            ]

    it "re-groups report by tag, nested tags" $

        (artistsReport # unwrap # Report.groupItemsByKind @ArtistTag KGenre # Report.unfold)
        `shouldEqual`
        [ S /\
            [ G [ "Analogue", "Grunge" ] /\ (A <$> [ "Nirvana" ])
            , G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
            , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen", "Depeche Mode" ])
            , G [ "Electronic" ] /\ (A <$> [ "Moby", "GusGus", "The Knife", "Fever Ray", "Depeche Mode" ])
            , G [ "Electronic", "Beats", "Big Beat" ] /\ (A <$> [ "The Chemical Brothers", "The Prodigy" ])
            , G [ "Electronic", "Beats", "Break Beat" ] /\ (A <$> [ "The Chemical Brothers", "The Prodigy" ])
            , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
            , G [ "Electronic", "Synth Pop" ] /\ (A <$> [ "The Knife", "Depeche Mode" ])
            ]
        ]

        <>

        (artistsReport # unwrap # Report.groupItemsByKind @ArtistTag KCountry # Report.unfold)
        `shouldEqual`
        [ S /\
            [ G [ "Americas", "USA" ] /\ (A <$> [ "NIN", "Nirvana", "Moby" ])
            , G [ "Europe", "Germany" ] /\ (A <$> [ "Rammstein" ])
            , G [ "Europe", "Sweden" ] /\ (A <$> [ "The Knife", "Fever Ray" ])
            , G [ "Europe", "UK" ] /\ (A <$> [ "Queen", "The Chemical Brothers", "The Prodigy", "Massive Attack", "Depeche Mode" ])
            , G [ "Iceland" ] /\ (A <$> [ "GusGus" ])
            ]
        ]

        <>

        (artistsReport # unwrap # Report.groupItemsByKind @ArtistTag KAlbumsCount # Report.unfold)
        `shouldEqual`
        [ S /\
            [ G [ "Less-than-5" ] /\ (A <$> [ "Nirvana", "Fever Ray" ])
            , G [ "More-than-10" ] /\ (A <$> [ "NIN", "Queen", "The Chemical Brothers", "GusGus", "Depeche Mode" ])
            , G [ "More-than-20" ] /\ (A <$> [ "Moby" ])
            , G [ "More-than-5" ] /\ (A <$> [ "Rammstein", "The Prodigy", "Massive Attack", "The Knife" ])
            ]
        ]

    it "re-groups report by tag, nested tags + unfold all" $

        (artistsReport # unwrap # Report.groupItemsByKind @ArtistTag KGenre # Report.unfoldAll)
        `shouldEqual`
        [ S /\
            [ G [ "Analogue" ] /\ (A <$> [])
            , G [ "Analogue", "Grunge" ] /\ (A <$> [ "Nirvana" ])
            , G [ "Analogue", "Rock" ] /\ (A <$> [ "Queen", "Rammstein" ])
            , G [ "Analogue", "Rock", "Pop Rock" ] /\ (A <$> [ "Queen", "Depeche Mode" ])
            , G [ "Electronic" ] /\ (A <$> [ "Moby", "GusGus", "The Knife", "Fever Ray", "Depeche Mode" ])
            , G [ "Electronic", "Beats" ] /\ (A <$> [])
            , G [ "Electronic", "Beats", "Big Beat" ] /\ (A <$> [ "The Chemical Brothers", "The Prodigy" ])
            , G [ "Electronic", "Beats", "Break Beat" ] /\ (A <$> [ "The Chemical Brothers", "The Prodigy" ])
            , G [ "Electronic", "Industrial" ] /\ (A <$> [ "NIN", "Rammstein" ])
            , G [ "Electronic", "Synth Pop" ] /\ (A <$> [ "The Knife", "Depeche Mode" ])
            ]
        ]

        <>

        (artistsReport # unwrap # Report.groupItemsByKind @ArtistTag KCountry # Report.unfoldAll)
        `shouldEqual`
        [ S /\
            [ G [ "Americas" ] /\ (A <$> [])
            , G [ "Americas", "USA" ] /\ (A <$> [ "NIN", "Nirvana", "Moby" ])
            , G [ "Europe" ] /\ (A <$> [])
            , G [ "Europe", "Germany" ] /\ (A <$> [ "Rammstein" ])
            , G [ "Europe", "Sweden" ] /\ (A <$> [ "The Knife", "Fever Ray" ])
            , G [ "Europe", "UK" ] /\ (A <$> [ "Queen", "The Chemical Brothers", "The Prodigy", "Massive Attack", "Depeche Mode" ])
            , G [ "Iceland" ] /\ (A <$> [ "GusGus" ])
            ]
        ]


  describe "converting to tree" $ do
    it "properly converts storage to tree (no sorting)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree sampleReportA)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        noSortingTreeSampleA

    it "properly converts storage to plain tree (no sorting)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toPlainTree sampleReportA)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        plainTreeSampleA

    it "properly artists report to tree (no sorting)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree $ unwrap $ artistsReport)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        artistsReportSampleA

    it "properly artists report to tree (sort by genre)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree $ Report.groupItemsByKind @ArtistTag KGenre $ unwrap $ artistsReport)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        artistsReportSampleByGenre

    it "properly artists report to tree (sort by country)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree $ Report.groupItemsByKind @ArtistTag KCountry $ unwrap $ artistsReport)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        artistsReportSampleByCountry

    it "properly artists report to tree (sort by albums count)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree $ Report.groupItemsByKind @ArtistTag KAlbumsCount $ unwrap $ artistsReport)
        -- (Tree.toString Mode.Dashes identity $ Storage.toTree sampleStorage)
        `U.shouldEqual`
        artistsReportSampleByAlbumsCount


noSortingTreeSampleA = """*
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
┊┄┄┄┄I: "group-2-1-1-item-2"""" :: String


sortedTreeSampleA = """*
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
┊┄┄┄┄I: "group-2-1-1-item-2"""" :: String


plainTreeSampleA = """*
┊S: "subject1"
┊┄G: ["group-1"]
┊┄┄I: "group-1-item-1"
┊┄┄I: "group-1-item-2"
┊┄G: ["group-1","group-1-1"]
┊┄┄I: "group-1-1-item-1"
┊┄┄I: "group-1-1-item-2"
┊┄G: ["group-1","group-1-1","group-1-1-1"]
┊┄┄I: "group-1-1-1-item-1"
┊┄┄I: "group-1-1-1-item-2"
┊┄G: ["group-1","group-1-2"]
┊┄┄I: "group-1-2-item-1"
┊┄┄I: "group-1-2-item-2"
┊┄G: ["group-1","group-1-3"]
┊┄┄I: "group-1-3-item-1"
┊┄┄I: "group-1-3-item-2"
┊┄G: ["group-1","group-1-4"]
┊┄G: ["group-1","group-1-4","group-1-4-1"]
┊┄┄I: "group-1-4-1-item-1"
┊┄┄I: "group-1-4-1-item-2"
┊┄G: ["group-1","group-1-4","group-1-4-2"]
┊┄┄I: "group-1-4-2-item-1"
┊┄┄I: "group-1-4-2-item-2"
┊┄G: ["group-1","group-1-4","group-1-4-2","group-1-4-2-1"]
┊┄┄I: "group-1-4-2-1-item-1"
┊┄G: ["group-1","group-1-4","group-1-4-3"]
┊┄G: ["group-1","group-1-4","group-1-4-4"]
┊┄┄I: "group-1-4-4-item-1"
┊┄G: ["group-2"]
┊┄┄I: "group-2-item-1"
┊┄┄I: "group-1-item-2"
┊┄G: ["group-2","group-2-1"]
┊┄┄I: "group-2-1-item-1"
┊┄┄I: "group-2-1-item-2"
┊┄G: ["group-2","group-2-1","group-2-1-1"]
┊┄┄I: "group-2-1-1-item-1"
┊┄┄I: "group-2-1-1-item-2"
┊┄G: ["group-1","group-2-2"]""" :: String


artistsReportSampleA = """*
┊S: subj
┊┄G: ["root"]
┊┄┄I: "NIN"
┊┄┄I: "Queen"
┊┄┄I: "Rammstein"
┊┄┄I: "The Chemical Brothers"
┊┄┄I: "The Prodigy"
┊┄┄I: "Nirvana"
┊┄┄I: "Moby"
┊┄┄I: "Massive Attack"
┊┄┄I: "GusGus"
┊┄┄I: "The Knife"
┊┄┄I: "Fever Ray"
┊┄┄I: "Depeche Mode"""" :: String


artistsReportSampleByGenre = """*
┊S: subj
┊┄G: ["Analogue"]
┊┄┄G: ["Analogue","Grunge"]
┊┄┄┄I: "Nirvana"
┊┄┄G: ["Analogue","Rock"]
┊┄┄┄I: "Queen"
┊┄┄┄I: "Rammstein"
┊┄┄┄G: ["Analogue","Rock","Pop Rock"]
┊┄┄┄┄I: "Queen"
┊┄┄┄┄I: "Depeche Mode"
┊┄G: ["Electronic"]
┊┄┄I: "Moby"
┊┄┄I: "GusGus"
┊┄┄I: "The Knife"
┊┄┄I: "Fever Ray"
┊┄┄I: "Depeche Mode"
┊┄┄G: ["Electronic","Beats"]
┊┄┄┄G: ["Electronic","Beats","Big Beat"]
┊┄┄┄┄I: "The Chemical Brothers"
┊┄┄┄┄I: "The Prodigy"
┊┄┄┄G: ["Electronic","Beats","Break Beat"]
┊┄┄┄┄I: "The Chemical Brothers"
┊┄┄┄┄I: "The Prodigy"
┊┄┄G: ["Electronic","Industrial"]
┊┄┄┄I: "NIN"
┊┄┄┄I: "Rammstein"
┊┄┄G: ["Electronic","Synth Pop"]
┊┄┄┄I: "The Knife"
┊┄┄┄I: "Depeche Mode"""" :: String


artistsReportSampleByCountry = """*
┊S: subj
┊┄G: ["Americas"]
┊┄┄G: ["Americas","USA"]
┊┄┄┄I: "NIN"
┊┄┄┄I: "Nirvana"
┊┄┄┄I: "Moby"
┊┄G: ["Europe"]
┊┄┄G: ["Europe","Germany"]
┊┄┄┄I: "Rammstein"
┊┄┄G: ["Europe","Sweden"]
┊┄┄┄I: "The Knife"
┊┄┄┄I: "Fever Ray"
┊┄┄G: ["Europe","UK"]
┊┄┄┄I: "Queen"
┊┄┄┄I: "The Chemical Brothers"
┊┄┄┄I: "The Prodigy"
┊┄┄┄I: "Massive Attack"
┊┄┄┄I: "Depeche Mode"
┊┄G: ["Iceland"]
┊┄┄I: "GusGus"""" :: String


artistsReportSampleByAlbumsCount = """*
┊S: subj
┊┄G: ["Less-than-5"]
┊┄┄I: "Nirvana"
┊┄┄I: "Fever Ray"
┊┄G: ["More-than-10"]
┊┄┄I: "NIN"
┊┄┄I: "Queen"
┊┄┄I: "The Chemical Brothers"
┊┄┄I: "GusGus"
┊┄┄I: "Depeche Mode"
┊┄G: ["More-than-20"]
┊┄┄I: "Moby"
┊┄G: ["More-than-5"]
┊┄┄I: "Rammstein"
┊┄┄I: "The Prodigy"
┊┄┄I: "Massive Attack"
┊┄┄I: "The Knife"""" :: String
