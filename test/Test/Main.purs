module Test.Main where

import Prelude


import Effect.Class (liftEffect)

import Yoga.JSON (readJSON) as JSON
import Foreign (ForeignError, renderForeignError) as F


import Data.Either (Either, either)
import Data.Newtype (unwrap)
import Data.Map (empty) as Map
import Data.List.NonEmpty as NEL
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Text.Diff

import Test.Spec (Spec, it, itOnly, describe)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Effect (Effect)
import Effect.Aff (launchAff_, delay)

import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Utils (shouldEqual) as U

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (GameId(..), GameTag, gameName) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray) as GL
import GameLog.Types.Achievement (Tag) as GL
import Report (toReport)

import Report.Export.DhallDodo (toDhall) as D


onlyFewGamesFilePath = "test/games-samples/only-few-games.json" :: String


type FE a = Either (NEL.NonEmptyList F.ForeignError) a


main :: Effect Unit
main = runSpecAndExitProcess [consoleReporter] do
  describe "convert" do
      it "dhall" do
        fileText <- liftEffect $ readTextFile UTF8 onlyFewGamesFilePath
        let (eDhallGameCollection :: FE GL.FromDhall) = JSON.readJSON fileText
        either
            (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
            (\dhallGameCollection -> do
                let gameCollection  = GL.dhallToAchievements dhallGameCollection
                let (glReport :: GL.GamesReport) = toReport $ GL.fromArray gameCollection
                let reportDhall = D.toDhall @GL.GameId @GL.GameTag @GL.Tag glReport
                "isAwesome" `U.shouldEqual` "isntAwesome"
            )
            eDhallGameCollection

        -- either
        --     (\errors -> fail $ "Errors: " <> (String.joinWith "\n" $ F.renderForeignError <$> NEL.toUnfoldable errors))
        --     (\achs -> "isAwesome" `U.shouldEqual` "isAwesome")
        --     reportDhall


expectedDhall = """
""" :: String