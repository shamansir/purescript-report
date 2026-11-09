module Report.Web.Tabular where

import Prelude

import Data.Newtype (unwrap, wrap)
import Data.Array (length, intersperse) as Array
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.Core (SDate(..), toLeadingZero, dateFromRec) as CT
import Report.Class as S
import Report.Tabular (Tabular)
import Report.Tabular as Tabular
import Report.Decorator (fromArray') as Decorators
import Report.AtomicValue (AtomicValue(..)) as AV
import Report.Decorators.Tabular.TabularValue (TabularValue(..))
import Report.Decorators.Tabular.TabularValue (toDecorator) as TV
import Report.Decorators.Tabular.Logbook (entries) as Logbook
import Report.Decorators.Tags (RawTag)

import Report.Web.CSS as CSS
import Report.Web.Helpers
import Report.Web.Helpers.InlineOrBlock
import Report.Web.Decorators (renderDecorator, renderTags)

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
                [ HP.style CSS.subjectTabularValues ]
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


renderTabular :: forall w. Tabular TabularValue -> H w Unit
renderTabular = Tabular.items >>> \tabularItems ->
    if Array.length tabularItems > 0 then
            HH.span
                [ HP.style CSS.tabularBlock ]
                $ HH.span [ HP.style CSS.tabularRow ] <$> pure <$> renderTabularValue <$> tabularItems
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
        TVLogbook logbook ->
            nestValues (renderLogbookEntry key) $ Logbook.entries logbook
        where
            wrapInnerTabular = HH.div [ HP.style CSS.tabularInnerNest ]
            renderLogbookEntry key { name, task, start, end } =
                HH.div_
                    $ [ renderTabularAtomicValue $ mkItem key name $ AV.PTask task ]
                    <> maybe [] (pure <<< renderTabularAtomicValue <<< mkItem key "start" <<< AV.OnDate <<< CT.dateFromRec) start
                    <> maybe [] (pure <<< renderTabularAtomicValue <<< mkItem key "end" <<< AV.OnDate <<< CT.dateFromRec) end
            mkItem :: forall a. String -> String -> a -> Tabular.Item a
            mkItem key label v = Tabular.Item { key, label, value : v }
            nestValues :: forall a. (a -> H w Unit) -> Array a -> H w Unit
            nestValues renderF valuesArr =
                HH.div
                    [ HP.style CSS.tabularNest ]
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


renderTabularAtomicValue :: forall w. Tabular.Item AV.AtomicValue -> H w Unit -- TODO: InlineOrBlock
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
                AV.PText text ->
                    qcolorSpan textColor text
                AV.PNumber num ->
                    qcolorSpan numColor $ formatNum num
                AV.PInt int ->
                    qcolorSpan numColor $ formatInt int
                AV.PYear int ->
                    qcolorSpan numColor $ show int
                AV.PID int ->
                    qcolorSpan numColor $ show int
                AV.PBoolean bool ->
                    qcompleteCheckbox bool
                AV.OnTime ({ hrs, min, sec }) ->
                    HH.span_
                        [ qcolorSpan timeColor $ CT.toLeadingZero hrs
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero min
                        , qtimesplitSpan
                        , qcolorSpan timeColor $ CT.toLeadingZero sec
                        ]
                AV.OnDate (CT.SDate { day, month, year }) ->
                    HH.span_
                        [ qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
                AV.OnDateTime date time ->
                    HH.span_
                        [ valueSpan (AV.OnDate date)
                        , qspacerSpan
                        , valueSpan (AV.OnTime time)
                        ]
                AV.TimeRange { from, to } ->
                    HH.span_
                        [ valueSpan (AV.OnTime from)
                        -- , qspacerSpan
                        , qrangesplitSpan
                        -- , qspacerSpan
                        , valueSpan (AV.OnTime to)
                        ]
                AV.DateRange { from, to } ->
                    HH.span_
                        [ valueSpan (AV.OnDate from)
                        -- , qspacerSpan
                        , qrangesplitSpan
                        -- , qspacerSpan
                        , valueSpan (AV.OnDate to)
                        ]
                AV.DateTimeRange { from, to } ->
                    HH.span_
                        [ valueSpan (AV.OnDate from.date)
                        , qspacerSpan
                        , valueSpan (AV.OnTime from.time)
                        , qrangesplitSpan
                        , valueSpan (AV.OnDate to.date)
                        , qspacerSpan
                        , valueSpan (AV.OnTime to.time)
                        ]
                AV.PRawTags tags ->
                    renderTags
                        (wrap $ unwrap tags)
                        { isEditingTags : Nothing
                        , isSelected : false
                        , noop : unit
                        , onCancelEditing : unit
                        , onClick : const unit
                        , onEdit : const unit
                        , onStartEditing : const unit
                        , onTagClick : const $ const unit
                        }
                    {-
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
                    -}
                aval ->
                    let decorator = TV.toDecorator aval in
                    fromMaybe (HH.text "BLOCK (TODO)")
                    $ loadInlineContent $ Tuple.snd $ renderDecorator -- TODO: use InlineOrBlock
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
                        , parentItemName : ""
                        , allDecorators : Decorators.fromArray' [ decorator ]
                        }