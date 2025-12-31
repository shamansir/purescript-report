module Report.Web.Tabular where

import Prelude

import Data.Newtype (unwrap)
import Data.Array (length) as Array

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Class as S
import Report.Tabular as Tabular
import Report.Modifiers.Tabular.TabularValue (TabularValue(..))

import Report.Web.Helpers


renderTabularValues
    :: forall @item_tag item w i
     . S.IsItem item_tag item
    -- => SuffixesRenderConfig i
    => item
    -> H w i
renderTabularValues item =
    let
        i_tabular = S.i_tabular @item_tag item
        tabularItems = Tabular.items i_tabular
    in
        if Array.length tabularItems > 0 then
            HH.span
                [ HP.style "display: block; margin: 0 0 0 50px; color: royalblue; font-size: 0.8em;" ]
                $ tabularItems <#> renderTabularValue
        else
            HH.span_ []



renderTabularValue :: forall w i. Tabular.Item TabularValue -> H w i
renderTabularValue = unwrap >>> \{ key, label, value } ->
    HH.span_
        [ qcolorSpan tabularLabelColor label
        , qspacerSpan
        , qtabularsplitSpan
        , valueSpan value
        ]
    where
        valueSpan =
            case _ of
                TVString text ->
                    qcolorSpan textColor text
                TVNumber num ->
                    qcolorSpan numColor $ formatNum num
                TVBoolean bool ->
                    qcompleteCheckbox bool
                TVTime ({ hrs, min, sec }) ->
                    HH.span_
                        [ qcolorSpan timeColor $ CT.toLeadingZero hrs
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero min
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero sec
                        ]
                TVDate (CT.SDate { day, month, year }) ->
                    HH.span_
                        [ qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
                TVDateTime date time ->
                    HH.span_
                        [ valueSpan (TVDate date)
                        , qspacerSpan
                        , valueSpan (TVTime time)
                        ]
                TVTimeRange { from, to } ->
                    HH.span_
                        [ valueSpan (TVTime from)
                        -- , qspacerSpan
                        , qrangesplitSpan
                        -- , qspacerSpan
                        , valueSpan (TVTime to)
                        ]
                TVDateRange { from, to } ->
                    HH.span_
                        [ valueSpan (TVDate from)
                        -- , qspacerSpan
                        , qrangesplitSpan
                        -- , qspacerSpan
                        , valueSpan (TVDate to)
                        ]
                TVDateTimeRange { from, to } ->
                    HH.span_
                        [ valueSpan (TVDate from.date)
                        , qspacerSpan
                        , valueSpan (TVTime from.time)
                        , qrangesplitSpan
                        , valueSpan (TVDate to.date)
                        , qspacerSpan
                        , valueSpan (TVTime to.time)
                        ]