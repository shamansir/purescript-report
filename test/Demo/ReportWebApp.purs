module Demo.ReportWebApp where

import Prelude

import Type.Proxy (Proxy(..))

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log) as Console
import Effect.Aff.Class (class MonadAff)

import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.Tuple (uncurry) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Newtype (wrap)

import Fetch (Method(..), fetch)  as F
import Fetch.Yoga.Json (fromJSON) as F

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

import GameLog.Dhall (FromDhall, dhallToAchievements) as GL
import GameLog.Types.Game (Game, GameId(..), GameTag, gameName) as GL
import GameLog.Types.SingleGameStats (totalAchievements) as GL
import GameLog.Types.ManyGamesStats (GamesReport, fromArray, RawAchievements) as GL
import GameLog.Types.Achievement (Achievement, Tag, TagKind) as GL

import Report (toReport)
import Report (build) as Report
import Report.Group (Group) as Report
import Report.Modify (RecalculateInclude(..))
import Report.Decorators.Stats.Collect (CollectWhat(..))
import Report.Decorators.Tags (RawTag, RawTagKind)
import Report.Impl.Subject (Subject, SubjectId(..)) as Impl
import Report.Impl.Group (Group) as Impl
import Report.Impl.Item (Item) as Impl
import Report.Impl.Q.Subject as QS
import Report.Impl.Q.Group as QG
import Report.Impl.Q.Item as QI
import Report.Web.Component as StatsReport
import Report.Web.Component.RecalcBehavior
import Report.Convert.Generic (RR(..))


main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body


type Slots =
    ( {- tree :: YST.Slot WS.SourceKey
    , -} report :: forall q o. H.Slot q o Unit
    )


type State =
    { report :: Maybe RR
    }


_report  = Proxy :: _ "report"


data Action
    = Skip
    | Initialize


initialReport :: RR
initialReport = RR $ Report.build
    [ QS.mk (Impl.SubjectId "subject-1") "Subject 1"
        /\  [ QG.mk [ "group-1" ] "S1: Group 1"
                /\ [ QI.mk "S1G1: Item 1", QI.mk "S1G1: Item 2", QI.mk "S1G1: Item 3" ]
            , QG.mk [ "group-2" ] "S1: Group 2"
                /\ [ QI.mk "S1G2: Item 1", QI.mk "S1G2: Item 2", QI.mk "S1G2: Item 3", QI.mk "S1G2: Item 4" ]
            ]
    , QS.mk (Impl.SubjectId "subject-2") "Subject 2"
        /\  [ QG.mk [ "group-1" ] "S2: Group 1"
                /\ [ QI.mk "S2G1: Item 1", QI.mk "S2G1: Item 2", QI.mk "S2G1: Item 3" ]
            , QG.mk [ "group-2" ] "S2: Group 2"
                /\ [ QI.mk "S2G2: Item 1", QI.mk "S1G2: Item 2", QI.mk "S2G2: Item 3", QI.mk "S2G2: Item 4" ]
            ]
    ]


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
                [ case s.report of
                    Just report ->
                        HH.slot_ _report unit reportComponent report
                    Nothing ->
                        HH.div_ [ HH.text "Loading report..." ]
                ]

        handleAction = case _ of
            Skip -> H.modify_ \s -> s
            Initialize -> do
                H.modify_ \s -> s { report = Just initialReport }


reportComponent :: forall query output m. MonadEffect m => H.Component query RR output m
reportComponent =
    StatsReport.component @RR @Impl.SubjectId @RawTag @RawTagKind @RawTag @(Impl.Subject Impl.SubjectId RawTag) @Impl.Group @(Impl.Item RawTag) $
        StatsReport.defaultConfig
            { preSelected = [ ]
            , recalculate = onAnyAction { collect : ItemsProgress, include : OnlyDirect }
            }