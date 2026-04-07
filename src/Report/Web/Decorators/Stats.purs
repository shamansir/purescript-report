module Report.Web.Decorators.Stats where

import Prelude

import Data.Int (toNumber) as Int
import Data.Array (length) as Array
import Data.FunctorWithIndex (mapWithIndex)

import Report.Decorators.Progress (NProgress(..))
import Report.Decorators.Stats (Stats, GotTotal(..), gotTotalFromStats) as S

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Elements as HS
import Halogen.Svg.Attributes as HA
import Halogen.Svg.Attributes.Color as HAC

import Report.Web.Helpers
import Report.Web.Decorators.Progress (percentage')


renderGroupStats :: forall w i. S.Stats -> H w i
renderGroupStats = S.gotTotalFromStats >>> case _ of
    S.Defined { got, total } ->
        let
            isDone = got >= total
        in
            HH.div
                [ HP.style "opacity: 0.55; max-width: 60%;" ]
                [ {- qitemmarkerSpan $ if isDone then completeColor else incompleteColor
                , qspacerSpan
                , -} percentage' groupProgressBarCompleteColor groupProgressBarIncompleteColor (Int.toNumber total) (Int.toNumber got)
                , qcolorSpan (if isDone then completeColor else incompleteColor) $ "(" <> formatInt got <> "/" <> formatInt total <> ")"
                ]
        -- HH.span
        --     [ HP.style "font-size: 0.8em; opacity: 0.8; margin: 5px 0;" ]
        --     [ HH.text $ " (" <> show got <> "/" <> show total <> ")" ]
    S.JustCount { count } ->
        HH.div
            [ HP.style "display: inline-block; position: relative; top: 1px;" ]
            [ qcolorSpan completeColor $ "(" <> formatInt count <> ")" ]
    S.GTStatsValue -> HH.text ""
    S.Undefined -> HH.text ""


gotTotalBadge :: forall r w i. { got :: Int, total :: Int | r } -> H w i
gotTotalBadge { got, total } =
    HH.span
        [ HP.style "font-size: 0.8em; opacity: 0.8; margin: 5px 0;" ]
        [ HH.text $ " (" <> show got <> "/" <> show total <> ")" ]


renderProgressPlates :: forall w i. Array NProgress -> H w i
renderProgressPlates itemsProgress =
    let
        platesCount = Array.length itemsProgress
        platesByV = 5
        platesByH' = platesCount `div` platesByV
        platesByH = if platesByH' * platesByV == platesCount then platesByH' else platesByH' + 1

        renderSquare idx color =
            HS.rect
                [ HA.x $ (Int.toNumber $ idx `div` platesByH) * 10.0
                , HA.y $ (Int.toNumber $ idx `mod` platesByV) * 10.0
                , HA.width 10.0, HA.height 10.0
                , HA.rx 2.0, HA.ry 2.0
                , HA.fill color
                ]
        renderPlate idx = colorFor >>> renderSquare idx
    in
        HS.svg [] $ mapWithIndex renderPlate itemsProgress
    where
        colorFor = case _ of
            Achieved -> plateCompleteColor
            OnTheWay amount -> plateOnTheWayColor
            NotAchieved -> plateIncompleteColor
            StatsValue -> plateStatsValColor
            Skip -> plateSkipColor

