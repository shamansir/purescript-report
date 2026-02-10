module Report.Web.Modifiers.Stats where

import Prelude

import Data.Int (toNumber) as Int

import Report.Modifiers.Stats (Stats, GotTotal(..), gotTotalFromStats) as S

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.Web.Helpers
import Report.Web.Modifiers.Progress (percentage')


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
        HH.div [] [ qcolorSpan completeColor $ "(" <> formatInt count <> ")" ]
    S.GTStatsValue -> HH.text ""
    S.Undefined -> HH.text ""


gotTotalBadge :: forall r w i. { got :: Int, total :: Int | r } -> H w i
gotTotalBadge { got, total } =
    HH.span
        [ HP.style "font-size: 0.8em; opacity: 0.8; margin: 5px 0;" ]
        [ HH.text $ " (" <> show got <> "/" <> show total <> ")" ]
