module Report.Web.Decorators where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.KeyboardEvent as KE

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Core.Logic (EncodedValue(..), view, edit) as CT
import Report.Class as S
-- import Report.Prefix as Prefixes
-- import Report.Prefix (Key(..), Prefix(..)) as Prefix
import Report.Decorator (Decorators)
import Report.Decorator as Decorators
import Report.Decorator (Key(..), Decorator(..)) as Dec
import Report.Decorators.Rating (toNumber, maxValue, relValue, toStars, toString) as Rating
import Report.Decorators.Priority (priorityChar) as Priority
import Report.Decorators.Task (taskPToString) as Task
import Report.Decorators.Tags (Tags(..))

import Report.Web.Helpers
import Report.Web.Decorators.Types (DecoratorsRenderConfig, DecoratorRenderConfig)
import Report.Web.Decorators.Progress (renderProgress)
import Report.Web.Decorators.Tags (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)


renderPrefixes
    :: forall @item_tag item w i
     . S.IsTag item_tag
    => S.IsItem item
    => S.HasDecorators item_tag item
    => DecoratorsRenderConfig i item_tag
    -> item
    -> Array (H w i)
renderPrefixes conf item =
    let
        i_decorators = S.i_decorators @item_tag item
        i_name = S.i_name item
        decoratorsKeys = Decorators.keys i_decorators # Array.filter Decorators.isPrefix # Array.sortWith Decorators.orderOf -- FIXME: should be already sorted
    in
    renderDecorators_ @item_tag conf i_name i_decorators decoratorsKeys


renderSuffixes
    :: forall @item_tag item w i
     . S.IsTag item_tag
    => S.IsItem item
    => S.HasDecorators item_tag item
    => DecoratorsRenderConfig i item_tag
    -> item
    -> Array (H w i)
renderSuffixes conf item =
    let
        i_decorators = S.i_decorators @item_tag item
        i_name = S.i_name item
        decoratorsKeys = Decorators.keys i_decorators # Array.filter Decorators.isSuffix # Array.sortWith Decorators.orderOf -- FIXME: should be already sorted
    in
    renderDecorators_ @item_tag conf i_name i_decorators decoratorsKeys


renderDecorators_
    :: forall @item_tag w i
     . S.IsTag item_tag
    => DecoratorsRenderConfig i item_tag
    -> String
    -> Decorators item_tag
    -> Array Dec.Key
    -> Array (H w i)
renderDecorators_ conf itemName allDecorators decoratorsKeys =
    let
        -- i_decorators = S.i_decorators @item_tag item
        -- i_name = S.i_name item
        -- decoratorsKeys = Decorators.keys i_decorators  Array.filter Decorators.isPrefix >>> Array.sortBy Decorators.orderOf -- FIXME: should be already sorted
        isSelected prefixKey =
            case conf.mbSelectedDecorator of
                Just selectedKey -> selectedKey == prefixKey
                Nothing -> false
    in
        decoratorsKeys
            <#> \key -> renderDecorator
                            { key
                            , isSelected : isSelected key
                            , isEditingDecorator : conf.isEditingDecorator key
                            , isEditingItemName : conf.isEditingItemName
                            , onClick : conf.onClick key
                            , onEdit : conf.onEdit key
                            , onEditItemName : conf.onEditItemName
                            , allDecorators
                            , parentItemName : itemName
                            , onStartEditing : conf.onStartEditing
                            , onCancelEditing : conf.onCancelEditing
                            , onTagClick : conf.onTagClick
                            , noop : conf.noop
                            }


renderDecorator
    :: forall @t w i
     . S.IsTag t
    => DecoratorRenderConfig i t
    -> H w i
renderDecorator conf =
    let
        currentDecorator = Decorators.get conf.key conf.allDecorators -- FIXME: we could just filter key / value pairs out of map
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        smallerFont inside = HH.span [ HP.style "font-size: 0.6em;" ] $ pure inside
        wrapDecorator content =
            HH.span
                [ HP.style $ if conf.isSelected then selectedStyle else usualStyle
                , HE.onClick conf.onClick
                ]
                [ content ]
    in
    wrapDecorator $ case conf.key of
        Dec.KRating -> case currentDecorator of
            Just (Dec.PRating rating) -> HH.span_
                [ whenNotEditing $
                    let ratingColor' = ratingColor $ Rating.relValue rating
                    in HH.span [ HP.style "margin: 0px -4px 0px 7px; opacity: 0.9; position: relative; top: -1px;" ]
                        [ qcolorSpan ratingColor' $ Rating.toStars rating
                        , qthinspacerSpan
                        , smallerFont $ qcolorSpan ratingColor' $ Rating.toString rating
                        ]
                , qspacerSpan
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KPriority -> case currentDecorator of
            Just (Dec.PPriority priority) -> HH.span_
                [ whenNotEditing $
                    let priorityColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan priorityColor' $ Priority.priorityChar priority
                        , qbracketspan "]"
                        ]
                , qspacerSpan
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KTask -> case currentDecorator of
            Just (Dec.PTask task) -> HH.span_
                [ whenNotEditing $
                    let taskColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan taskColor' $ Task.taskPToString task
                        , qbracketspan "]"
                        ]
                , qspacerSpan
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KProgress _ -> case currentDecorator of
            Just (Dec.SProgress progress) ->
                renderProgress
                    progressConfig
                    ( maybe
                        CT.view
                        CT.edit
                        conf.isEditingItemName
                        conf.parentItemName
                            <#> \name -> { itemName : name }
                    )
                    ( maybe
                        CT.view
                        CT.edit
                        conf.isEditingDecorator
                        progress
                    )
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KEarnedAt -> case currentDecorator of
            Just (Dec.SEarnedAt (CT.SDate { day, month, year })) -> HH.span_
                [ qspacerSpan
                , whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                        [ qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KDescription -> case currentDecorator of
            Just (Dec.SDescription description) -> HH.span_
                [ qspacerSpan
                , whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KReference -> case currentDecorator of
            Just (Dec.SReference groupRef) ->
                whenNotEditing $ renderGroupRef groupRef
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Dec.KTags -> case currentDecorator of
            Just (Dec.STags (Tags tags)) ->
                whenNotEditing $ HH.span_ $ (\tag -> itemTagBadge (conf.onTagClick tag) tag) <$> tags
            Just _ -> HH.text ""
            Nothing -> HH.text ""
    where
        -- whenNotEditing :: H w i -> H w i
        -- whenNotEditing nonEditing = case conf.isEditingPrefix of
        --     Just (CT.EncodedValue value) ->
        --         HH.span [ HP.style "color: gray;" ] [ HH.text value ]
        --     Nothing -> nonEditing
        progressConfig =
            { onEdit : conf.onEdit
            , onClick : conf.onClick
            , onEditItemName : conf.onEditItemName
            , onStartEditing : conf.onStartEditing
            , onCancelEditing : conf.onCancelEditing
            , noop : conf.noop
            }
        whenNotEditing :: H w i -> H w i
        whenNotEditing nonEditing = case conf.isEditingDecorator of
            Just (CT.EncodedValue encVal) ->
                HH.input
                    [ HP.type_ HP.InputText
                    , HP.value encVal
                    , HE.onClick conf.onStartEditing
                    , HE.onValueChange (CT.EncodedValue >>> conf.onEdit)
                    , HE.onKeyUp (KE.code >>> -- Debug.spy "key up" >>>
                        \code -> if code == "Escape" || code == "Enter"
                            then conf.onCancelEditing
                            else conf.noop)
                    , HE.onBlur $ const conf.onCancelEditing
                    , HE.onAbort $ const conf.onCancelEditing
                    ]
            Nothing -> nonEditing