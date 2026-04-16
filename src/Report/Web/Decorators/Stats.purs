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
                [ HP.style "opacity: 0.55;" ]
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
        maxHeight = 50.0
        platesCount = Array.length itemsProgress
        platesByV =
            if platesCount <= 5 then 1
            -- else if platesCount <= 5 * 5 then platesCount `div` 5
            else 5
        platesByH' = platesCount `div` platesByV
        platesByH =
            if platesCount <= 5 * 5 then 5
            else if platesByH' * platesByV == platesCount then platesByH'
            else platesByH' + 1
        plateSide = 10.0 -- min 20.0 $ maxHeight / Int.toNumber platesByV
        -- svgWidth = if platesByV /= 0 then ((Int.toNumber platesCount / Int.toNumber platesByV) + 1.0) * plateSide else 0.0
        svgWidth = Int.toNumber platesByH * plateSide
        svgHeight = maxHeight

        renderSquare idx color =
            HS.rect
                [ HA.x $ (Int.toNumber $ idx `mod` platesByH) * plateSide
                , HA.y $ (Int.toNumber $ idx `div` platesByH) * plateSide
                , HA.width plateSide, HA.height plateSide
                , HA.rx 2.0, HA.ry 2.0
                , HA.fill color
                ]
        renderPlate idx = colorFor >>> renderSquare idx
    in
        HS.svg
            [ HA.width svgWidth, HA.height svgHeight
            , HP.style "display: inline-block; position: relative; align-self: center; margin-left: 12px;border:1px solid lightblue;border-radius:3px;background-color:ghostwhite;" --  opacity: 0.8;
            ]
            $ mapWithIndex renderPlate itemsProgress
    where
        colorFor = case _ of
            Achieved -> plateCompleteColor
            OnTheWay amount -> plateOnTheWayColor
            NotAchieved -> plateIncompleteColor
            StatsValue -> plateStatsValColor
            Skip -> plateSkipColor

