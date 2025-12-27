module Demo.App where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Effect.Aff.Class (class MonadAff)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array (length) as Array
import Data.String (joinWith) as String
import Data.Tuple (uncurry) as Tuple

import Test.Demo.Request as Request
import Fetch as F
import Fetch.Yoga.Json (fromJSON) as F

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML (input)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)

import GameLog.Dhall as GL
import GameLog.Types.Game as GL
import GameLog.Types.SingleGameStats as GL
import GameLog.Types.ManyGamesStats as GL
import GameLog.Types.Achievement as GL
import GameLog.Types.Game (GameId(..), GameTag) as GL

import Report (toReport)
import Report.Web.Component as StatsReport


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


type Slots =
  ( {- tree :: YST.Slot WS.SourceKey
  , -} report :: forall q o. H.Slot q o Unit
  )


type State =
    { report :: Maybe GL.GamesReport
    }


_report  = Proxy :: _ "report"


data Action
    = Skip
    | Initialize


component :: forall query input output m. MonadAff m => MonadEffect m => H.Component query input output m
component = H.mkComponent
    { initialState: \_ -> { report: Nothing }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }
    where
        render :: State -> H.ComponentHTML Action Slots m
        render s =
            HH.div_
                -- [ HH.h1_ [ HH.text "Hello, Halogen!" ]
                -- , HH.button
                --     [ HE.onClick \_ -> Skip
                --     ]
                --     [ HH.text "Click me" ]
                [ case s.report of
                    Just report ->
                        HH.slot_ _report unit reportComponent report
                    Nothing ->
                        HH.div_ [ HH.text "Loading report..." ]
                ]

        handleAction = case _ of
            Skip -> H.modify_ \s -> s
            Initialize -> do
                gameCollection <- H.liftAff $ do
                    { json } <- F.fetch "./games-collection.json"
                        { method: F.GET
                        , headers: { "Content-Type": "application/json" }
                        }

                    dhallGameCollection :: GL.FromDhall <- F.fromJSON json

                    let gameCollection  = GL.dhallToAchievements dhallGameCollection

                    liftEffect $ Console.log $ String.joinWith " :: "
                        $  Tuple.uncurry (\game achs -> GL.gameName game <> " " <> (show $ GL.totalAchievements achs))
                        <$> gameCollection

                    pure gameCollection
                H.modify_ \s -> s { report = Just $ toReport $ GL.fromArray gameCollection}


reportComponent :: forall query output m. MonadEffect m => H.Component query GL.GamesReport output m
reportComponent =
    StatsReport.component @GL.GameId @GL.GameTag @GL.Tag [ GL.DHL "astral-chain" ]