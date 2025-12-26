module Report.Web.Suffix.Stats where

import Prelude

import Data.Int (toNumber, floor) as Int

import Report.Prefix.Task (TaskP(..)) as S
import Report.Suffix.Stats (GotTotal(..), gotTotalFromStats, weightOf) as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Report.Web.Helpers
import Report.Web.Suffix.Progress


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
    S.GTStatsValue -> HH.text ""
    S.Undefined -> HH.text ""
