module Test.Report where

import Prelude

import Prelude

import Effect.Class (liftEffect)

import Debug as Debug


import Data.Either (either)
import Data.Newtype (class Newtype, unwrap)
import Data.Map (empty) as Map
import Data.Maybe (Maybe(..))
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.Tuple.Nested ((/\), type (/\))
import Data.Array ((:))
import Data.Array (reverse) as Array
import Data.Array.NonEmpty as NEA
import Data.Int (fromString) as Int

import Test.Spec (Spec, it, itOnly, describe, pending')
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
import Report.Chain as C
import Report.Builder as RB
import Report.Class
import Report.GroupPath (pathFromArray) as GP
import Report.Decorators.Stats (Stats(..)) as ST

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


data ArtistTag
    = Genre String
    | Country String
    | AlbumsCount Int
    -- | YearsActive Int Int


newtype MyGroup = G (Array String)
derive instance Newtype MyGroup _
derive newtype instance Show MyGroup
derive newtype instance Eq MyGroup
derive newtype instance Ord MyGroup


newtype TagTest1Item = TT1I String
derive instance Newtype TagTest1Item _
derive newtype instance Show TagTest1Item
derive newtype instance Eq TagTest1Item

newtype Artist = A String
derive instance Newtype Artist _
derive newtype instance Show Artist
derive newtype instance Eq Artist


instance HasTags BoolTag TagTest1Item where
    i_tags = unwrap >>> case _ of
        "item1" -> [ true ]
        "item2" -> [ false ]
        "item3" -> [ true ]
        "item4" -> [ false ]
        _ -> []
        >>> map BoolTag


instance IsGroup MyGroup where
    g_title = unwrap >>> map String.toUpper >>> String.joinWith "::"
    g_path = unwrap >>> GP.pathFromArray


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


instance HasTags ArtistTag Artist where
    i_tags = unwrap >>> case _ of
        "NIN" -> [ Country "USA", Genre "Industrial", AlbumsCount 15 ]
        "Queen" -> [ Country "UK", Genre "Rock", Genre "Pop Rock", AlbumsCount 15 ]
        "Rammstein" -> [ Country "Germany", Genre "Rock", Genre "Industrial", AlbumsCount 8 ]
        "The Chemical Brothers" -> [ Country "UK", Genre "Break Beat", Genre "Big Beat", AlbumsCount 10 ]
        "The Prodigy" -> [ Country "UK", Genre "Break Beat", Genre "Big Beat", AlbumsCount 7 ]
        "Nirvana" -> [ Country "USA", Genre "Grunge", AlbumsCount 3 ]
        "Moby" -> [ Country "USA", Genre "Electronic", AlbumsCount 23 ]
        "Massive Attack" -> [ Country "UK", Genre "Trip Hop", AlbumsCount 5 ]
        "GusGus" -> [ Country "Iceland", Genre "Electronic", AlbumsCount 12 ]
        "The Knife" -> [ Country "Sweden", Genre "Electronic", Genre "Synth Pop", AlbumsCount 5 ]
        "Fever Ray" -> [ Country "Sweden", Genre "Electronic", AlbumsCount 3 ]
        "Depeche Mode" -> [ Country "UK", Genre "Electronic", Genre "Synth Pop", Genre "Pop Rock", AlbumsCount 15 ]
        _ -> []


data ArtistTagKind
    = KGenre
    | KCountry
    | KAlbumsCount


derive instance Eq ArtistTagKind


instance IsSortable ArtistTagKind ArtistTag where
    kindOf = case _ of
        Genre _ -> KGenre
        Country _ -> KCountry
        AlbumsCount _ -> KAlbumsCount


instance Same ArtistTagKind where
    same = eq


{-
instance IsTag ArtistTag where
    tagContent = case _ of
        Genre _ -> C.End "Genre"
        Country _ -> C.End "Country"
        AlbumsCount _ -> C.End "Albums #"
    tagColors = const defaultTagColors
-}


{-
instance ConvertTo (C.Chain String) ArtistTag where
    convertTo = mkChainEncode $ case _ of
        Genre _ -> "genre"
        Country _ -> "country"
        AlbumsCount _ -> "albums_n"


instance ConvertFrom (C.Chain String) ArtistTag where
    convertFrom = mkChainDecode $ case _ of
        "genre" -> Just $ Genre "Sample"
        "country" -> Just $ Country "Sample"
        "albums_n" -> Just $ AlbumsCount $ -1
        _ -> Nothing
-}


{-
instance ConvertTo (C.Chain String) ArtistTagKind where
    convertTo = mkChainEncode $ case _ of
        KGenre -> "genre"
        KCountry -> "country"
        KAlbumsCount -> "albums_n"


instance ConvertFrom (C.Chain String) ArtistTagKind where
    convertFrom = mkChainDecode $ case _ of
        "genre" -> Just KGenre
        "country" -> Just KCountry
        "albums_n" -> Just KAlbumsCount
        _ -> Nothing
-}


instance IsGroupable MyGroup ArtistTag where
    t_group = case _ of
        Genre genre -> case genre of
            "Pop Rock" ->   Just $ C.More (G [ "Analogue" ]) $ C.More (G [ "Analogue", "Rock" ]) $ C.End $ G [ "Analogue", "Rock", "Pop Rock" ]
            "Rock" ->       Just $ C.More (G [ "Analogue" ]) $ C.End (G [ "Analogue", "Rock" ])
            "Grunge" ->     Just $ C.More (G [ "Analogue" ]) $ C.End (G [ "Analogue", "Grunge" ])
            "Electronic" -> Just $ C.End (G [ "Electronic" ])
            "Industrial" -> Just $ C.More (G [ "Electronic" ]) $ C.End $ G [ "Electronic", "Industrial" ]
            "Synth Pop" ->  Just $ C.More (G [ "Electronic" ]) $ C.End $ G [ "Electronic", "Synth Pop" ]
            "Break Beat" -> Just $ C.More (G [ "Electronic" ]) $ C.More (G [ "Electronic", "Beats" ]) $ C.End $ G [ "Electronic", "Beats", "Break Beat" ]
            "Big Beat" ->   Just $ C.More (G [ "Electronic" ]) $ C.More (G [ "Electronic", "Beats" ]) $ C.End $ G [ "Electronic", "Beats", "Big Beat" ]
            _ -> Nothing
        Country country ->
            case country of
                "UK" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "UK" ]
                "Germany" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "Germany" ]
                "Sweden" -> Just $ C.More (G [ "Europe" ]) $ C.End $ G [ "Europe", "Sweden" ]
                "USA" -> Just $ C.More (G [ "Americas" ]) $ C.End $ G [ "Americas", "USA" ]
                "Iceland" -> Just $ C.End (G [ "Iceland" ])
                _ -> Nothing
        AlbumsCount acount ->
            Just $
                if acount < 5 then C.End (G [ "Less-than-5" ])
                else if acount < 10 then C.End (G [ "More-than-5" ])
                else if acount < 20 then C.End (G [ "More-than-10" ])
                else C.End (G [ "More-than-20" ])


{-
intToDecimals :: Int -> Array Int
intToDecimals = go [] >>> Array.reverse
    where
        go prev n =
            let nextPos = n `div` 10
            in if nextPos <= 0
                then 0 : prev
                else
                    if nextPos == 1 then 1 : prev
                    else go (nextPos : prev) nextPos
-}


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

    let
        artistsReport =
            RB.buildG
                [ "subj" /\
                    [ G [ "root" ] /\
                        (A <$>
                            [ "NIN", "Queen", "Rammstein", "The Chemical Brothers"
                            , "The Prodigy", "Nirvana", "Moby", "Massive Attack"
                            , "GusGus", "The Knife", "Fever Ray", "Depeche Mode"
                            ])
                    ]
                ]
                # Report.fromBuilder

    it "re-groups report by tag, nested tags" $

        (artistsReport # Report.groupItemsByKind @ArtistTag KGenre # Report.unfold)
        `shouldEqual`
        [ "subj" /\
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

        (artistsReport # Report.groupItemsByKind @ArtistTag KCountry # Report.unfold)
        `shouldEqual`
        [ "subj" /\
            [ G [ "Americas", "USA" ] /\ (A <$> [ "NIN", "Nirvana", "Moby" ])
            , G [ "Europe", "Germany" ] /\ (A <$> [ "Rammstein" ])
            , G [ "Europe", "Sweden" ] /\ (A <$> [ "The Knife", "Fever Ray" ])
            , G [ "Europe", "UK" ] /\ (A <$> [ "Queen", "The Chemical Brothers", "The Prodigy", "Massive Attack", "Depeche Mode" ])
            , G [ "Iceland" ] /\ (A <$> [ "GusGus" ])
            ]
        ]

        <>

        (artistsReport # Report.groupItemsByKind @ArtistTag KAlbumsCount # Report.unfold)
        `shouldEqual`
        [ "subj" /\
            [ G [ "Less-than-5" ] /\ (A <$> [ "Nirvana", "Fever Ray" ])
            , G [ "More-than-10" ] /\ (A <$> [ "NIN", "Queen", "The Chemical Brothers", "GusGus", "Depeche Mode" ])
            , G [ "More-than-20" ] /\ (A <$> [ "Moby" ])
            , G [ "More-than-5" ] /\ (A <$> [ "Rammstein", "The Prodigy", "Massive Attack", "The Knife" ])
            ]
        ]

    it "re-groups report by tag, nested tags + unfold all" $

        (artistsReport # Report.groupItemsByKind @ArtistTag KGenre # Report.unfoldAll)
        `shouldEqual`
        [ "subj" /\
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

        (artistsReport # Report.groupItemsByKind @ArtistTag KCountry # Report.unfoldAll)
        `shouldEqual`
        [ "subj" /\
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
    pending' "properly converts storage to tree (no sorting)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree sampleReportA)
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

    pending' "properly converts storage to tree (with sorting)" $ do
        (Tree.toString Mode.Dashes (RB.nodeToString true) $ Report.toTree sampleReportA)
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