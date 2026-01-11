module Test.OrgDodo where

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
import Report.Convert.Org (toOrg) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


spec :: Spec Unit
spec =
    describe "convert" do
      it "org" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toOrg @GL.RawAchievements @GL.GameId @GL.GameTag @GL.Tag R.includeAll glReport
                reportDhall `U.shouldEqual` expectedDhall
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhall = """* Astral Chain
:PROPERTIES:
:Id: DHL:astral-chain
:Platform: TODO
:Playtime: TODO
:TrackedAt: <2025-08-12>
:END:
** File
:PROPERTIES:
:Path: 00-file
:Index: 0
:END:
*** Time : 7:54:00
*** Duration : 36:48:56
*** Date : 2001-01-11
*** Name : File 11
*** Chapter : Reckoning
*** Rank : Riotsu
*** Grade : Expert
*** Play Style : Pt Standard
** Stats
*** Order Completion : 67/185
** Hero
*** Health : 1200/1200
*** Resonance : 100.0%
*** ATK : 50
*** Money : 628G
*** Gene Codes : 514
*** Duty Evaluation : 41832
*** Order Completion : 36.0%
** Weapons
*** X-Baton Level : 6
*** Baton Atk : 130
*** Blaster Atk : 58
*** Gladius Atk : 282
*** Limiter Refill : 110.0%
*** Command Skills Unlocked : 5
*** Legatus Level : 4
*** AED Recovery : 100.0%
*** AED Speed : 7sec
*** Limiter Max : 130
*** Additional AED Batteries : 1
** Basic
*** [ ] All Pure Platinum
*** Red Cases Closed : 90
:PROPERTIES:
:LEVEL1: 1
:LEVEL2: 25
:LEVEL3: 50
:LEVEL4: 131
:END:
*** Blue Cases Closed : 68
:PROPERTIES:
:LEVEL1: 1
:LEVEL2: 25
:LEVEL3: 50
:LEVEL4: 87
:END:
*** [ ] Sword Scholar
*** [ ] Sword Master
*** [X] Arrow Scholar
*** [ ] Arrow Master
*** [X] Arm Scholar
*** [ ] Arm Master
*** [X] Beast Scholar
*** [ ] Beast Master
*** [ ] Axe Scholar
*** [ ] Axe Master
*** Legionis Dominus : 1/1
*** X-Baton Master : 1/1
*** Legatus Master : 1/1
""" :: String