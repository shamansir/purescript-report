module Test.TextDodo where

import Prelude

import Effect.Class (liftEffect)

import Yoga.JSON (readJSON) as JSON
import Foreign (ForeignError, renderForeignError) as F

import Data.Either (Either, either)
import Data.List.NonEmpty as NEL
import Data.String as String

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (fail)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Test.Utils (shouldEqual) as U

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (GameId, GameTag) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray, RawAchievements) as GL
import GameLog.Types.Achievement (Tag) as GL

import Report (toReport)
import Report.Convert.Generic (includeAll) as R
import Report.Convert.Text (toText) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


spec :: Spec Unit
spec =
    describe "convert" do
      it "text (plain)" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toText @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportDhall `U.shouldEqual` expectedDhall
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhall = """# Astral Chain
    - TrackedAt: 12-Aug-2025

    0. File 00-file
        Time : 07:54:00
        Duration : 36:48:56
        Date : 11-Jan-2001
        Name : File 11
        Chapter : Reckoning
        Rank : Riotsu
        Grade : Expert
        Play Style : Pt Standard

    1. Stats 01-stats
        Order Completion : 67/185

        2. Hero 01-stats::00-hero
            Health : 1200--1200
            Resonance : 1.0%
            ATK : 50
            Money : 628G
            Gene Codes : 514
            Duty Evaluation : 41832
            Order Completion : 0.36%

        3. Weapons 01-stats::01-weapons
            X-Baton Level : 6
            Baton Atk : 130
            Blaster Atk : 58
            Gladius Atk : 282
            Limiter Refill : 1.1%
            Command Skills Unlocked : 5
            Legatus Level : 4
            AED Recovery : 1.0%
            AED Speed : 7sec
            Limiter Max : 130
            Additional AED Batteries : 1

        4. Basic 01-stats::02-basic
            All Pure Platinum : 0/1
            Red Cases Closed
            Blue Cases Closed
            Sword Scholar : 0/1
            Sword Master : 0/1
            Arrow Scholar : 1/1
            Arrow Master : 0/1
            Arm Scholar : 1/1
            Arm Master : 0/1
            Beast Scholar : 1/1
            Beast Master : 0/1
            Axe Scholar : 0/1
            Axe Master : 0/1
            Legionis Dominus : 0/0
            X-Baton Master : 0/0
            Legatus Master : 0/0

        5. Chapters 01-stats::02-basic::00-chapters
            Startup : 1/1
            Awake : 1/1
            Link : 1/1""" :: String