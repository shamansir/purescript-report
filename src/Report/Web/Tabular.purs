module Report.Web.Tabular where

import Prelude

import Data.Newtype (unwrap)
import Data.Array (length, intersperse) as Array
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Class as S
import Report.Tabular (Tabular)
import Report.Tabular as Tabular
import Report.Decorator (empty) as Decorators
import Report.Decorators.Tabular.TabularValue (TabularValue(..), TabularAtomicValue(..))
import Report.Decorators.Tags (RawTag)

import Report.Web.Helpers
import Report.Web.Decorators (renderDecorator)

import Report.Convert.Keyed (keyOf)


renderSubjectTabularValues
    :: forall item w
     . S.HasTabular item
    -- => SuffixesRenderConfig i
    => item
    -> H w Unit
renderSubjectTabularValues item =
    let
        i_tabular = S.i_tabular item
        tabularItems = Tabular.items i_tabular
    in
        if Array.length tabularItems > 0 then
            HH.span
                [ HP.style "display: block; margin: 0 0 14px 2px; color: royalblue; font-size: 0.8em; position: relative; top: -19px;" ]
                $ tabularItems <#> renderTabularValue
        else
            HH.span_ []


renderItemTabularValues
    :: forall item w
     . S.HasTabular item
    -- => SuffixesRenderConfig i
    => item
    -> H w Unit
renderItemTabularValues =
    S.i_tabular >>> renderTabular


tabularStyle = "display: block; margin: 0 0 0 25px; color: royalblue; font-size: 0.8em;" :: String
nestStyle = "border-left: 2px solid color-mix(in srgb, royalblue 20%, transparent); padding-left: 5px;" :: String
innerTabularStyle = "border-left: 2px solid color-mix(in srgb, darkgreen 20%, transparent); padding-left: 5px; margin: 10px 0;" :: String


renderTabular :: forall w. Tabular TabularValue -> H w Unit
renderTabular = Tabular.items >>> \tabularItems ->
    if Array.length tabularItems > 0 then
            HH.span
                [ HP.style tabularStyle ]
                $ HH.span [ HP.style "display: block;" ] <$> pure <$> renderTabularValue <$> tabularItems
        else
            HH.span_ []


renderTabularValue :: forall w. Tabular.Item TabularValue -> H w Unit
renderTabularValue = unwrap >>> \{ key, label, value } ->
    case value of
        TVAtomic av -> renderTabularAtomicValue $ mkItem key label av
        TVValues valuesArr ->
            nestValues_ key renderTabularAtomicValue valuesArr
        TVValuesNest nestings ->
            HH.div []
                $ Array.intersperse (HH.span_ [ HH.br_, HH.br_ ])
                $ nestValues_ key renderTabularAtomicValue <$> nestings
        TVTabulars tabulars ->
            nestValues (map TVAtomic >>> renderTabular) tabulars
        TVTabularsNest { direct, parts } ->
            HH.div
                []
                [ {- HH.text "TabularsNest"
                , -} nestValues (map TVAtomic >>> renderTabular >>> pure >>> wrapInnerTabular) direct
                , nestValues (renderParts key) parts
                ]
        TVPair tabValA tabValB ->
            HH.div
                []
                [ {- HH.text "Pair"
                    , -} renderTabularValue $ mkItem key ""  tabValA
                , renderTabularValue $ mkItem key "" tabValB
                ]
        where
            wrapInnerTabular = HH.div [ HP.style innerTabularStyle ]
            mkItem :: forall a. String -> String -> a -> Tabular.Item a
            mkItem key label v = Tabular.Item { key, label, value : v }
            nestValues :: forall a. (a -> H w Unit) -> Array a -> H w Unit
            nestValues renderF valuesArr =
                HH.div
                    [ HP.style nestStyle ]
                    [ {- HH.text "Nest"
                    , -} HH.div_ $ renderF <$> valuesArr
                    ]
            nestValues_ :: forall a. String -> (Tabular.Item a -> H w Unit) -> Array a -> H w Unit
            nestValues_ key renderF =
                nestValues (mkItem key "" >>> renderF)
            renderParts key (tabAV /\ subParts) =
                HH.div
                    []
                    [ {- HH.text "Parts"
                    , -} renderTabularAtomicValue $ mkItem key "" tabAV
                    , nestValues (map TVAtomic >>> renderTabular >>> pure >>> wrapInnerTabular) subParts
                    ]


renderTabularAtomicValue :: forall w. Tabular.Item TabularAtomicValue -> H w Unit
renderTabularAtomicValue = unwrap >>> \{ key, label, value } ->
    HH.span_
        [ qcolorSpan tabularLabelColor label
        , qspacerSpan
        , qtabularsplitSpan
        , qspacerSpan
        , valueSpan value
        ]
    where
        valueSpan =
            case _ of
                TVString text ->
                    qcolorSpan textColor text
                TVNumber num ->
                    qcolorSpan numColor $ formatNum num
                TVInt int ->
                    qcolorSpan numColor $ formatInt int
                TVYear int ->
                    qcolorSpan numColor $ show int
                TVID int ->
                    qcolorSpan numColor $ show int
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
                TVDecorator decorator ->
                    renderDecorator @RawTag
                        { isEditingDecorator : Nothing
                        , isEditingItemName : Nothing
                        , isSelected : false
                        , key : keyOf decorator
                        , noop : unit
                        , onCancelEditing : unit
                        , onClick : const unit
                        , onEdit : const unit
                        , onStartEditing : const unit
                        , onEditItemName : const unit
                        , onTagClick : const $ const unit
                        , parentItemName : ""
                        , allDecorators : Decorators.empty
                        }