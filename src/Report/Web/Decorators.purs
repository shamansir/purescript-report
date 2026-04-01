module Report.Web.Decorators where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Tuple (fst, snd) as Tuple

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
import Report.Decorators.Tags (toArray) as Tags

import Report.Web.Helpers
import Report.Web.Helpers.InlineOrBlock
import Report.Web.Decorators.Types (DecoratorsRenderConfig, DecoratorRenderConfig, TagsRenderConfig, EditableValueEvents)
import Report.Web.Decorators.Progress (renderProgress)
import Report.Web.Decorators.Tags (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Decorators.EditInput as EI
import Report.Web.Decorators.Types


renderPrefixes
    :: forall item w i
     . S.IsItem item
    => S.HasDecorators item
    => DecoratorsRenderConfig i
    -> item
    -> Array (InlineOrBlock w i)
renderPrefixes conf item =
    let
        i_decorators = S.i_decorators item
        i_title = S.i_title item
        decoratorsKeys = Decorators.keys i_decorators # Array.filter Decorators.isPrefix # Array.sortWith Decorators.orderOf -- FIXME: should be already sorted
    in
    renderDecorators_ conf i_title i_decorators decoratorsKeys


renderSuffixes
    :: forall item w i
     . S.IsItem item
    => S.HasDecorators item
    => DecoratorsRenderConfig i
    -> item
    -> Array (InlineOrBlock w i)
renderSuffixes conf item =
    let
        i_decorators = S.i_decorators item
        i_title = S.i_title item
        decoratorsKeys = Decorators.keys i_decorators # Array.filter Decorators.isSuffix # Array.sortWith Decorators.orderOf -- FIXME: should be already sorted
    in
    renderDecorators_ conf i_title i_decorators decoratorsKeys


renderDecorators_
    :: forall w i
     . DecoratorsRenderConfig i
    -> String
    -> Decorators
    -> Array Dec.Key
    -> Array (InlineOrBlock w i)
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
                            , noop : conf.noop
                            }


renderDecorator
    :: forall w i
     . DecoratorRenderConfig i
    -> InlineOrBlock w i
renderDecorator conf =
    let
        currentDecorator = Decorators.get conf.key conf.allDecorators -- FIXME: we could just filter key / value pairs out of map
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        smallerFont inside = HH.span [ HP.style "font-size: 0.6em;" ] $ pure inside
        wrapInlineContent content =
            HH.span
                [ HP.style $ if conf.isSelected then selectedStyle else usualStyle
                , HE.onClick conf.onClick
                ]
                [ content ]
        wrapRenderedDecorator = mapInlineContent wrapInlineContent
    in
    wrapRenderedDecorator $ case conf.key of
        Dec.KRating -> case currentDecorator of
            Just (Dec.PRating rating) -> Inline $ whenNotEditing $
                    let ratingColor' = ratingColor $ Rating.relValue rating
                    in HH.span [ HP.style "margin: 0px -4px 0px 7px; opacity: 0.9; position: relative; top: -1px;" ]
                        [ qcolorSpan ratingColor' $ Rating.toStars rating
                        , qthinspacerSpan
                        , smallerFont $ qcolorSpan ratingColor' $ Rating.toString rating
                        ]
            Just _ -> Skip
            Nothing -> Skip
        Dec.KPriority -> case currentDecorator of
            Just (Dec.PPriority priority) -> Inline $ whenNotEditing $
                    let priorityColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan priorityColor' $ Priority.priorityChar priority
                        , qbracketspan "]"
                        ]
            Just _ -> Skip
            Nothing -> Skip
        Dec.KTask -> case currentDecorator of
            Just (Dec.PTask task) ->
                Inline $ whenNotEditing $
                    let taskColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan taskColor' $ Task.taskPToString task
                        , qbracketspan "]"
                        ]
            Just _ -> Skip
            Nothing -> Skip
        Dec.KProgress _ -> case currentDecorator of
            Just (Dec.SProgress progress) ->
                Tuple.snd $ renderProgress
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
            Just _ -> Skip
            Nothing -> Skip
        Dec.KEarnedAt -> case currentDecorator of
            Just (Dec.SEarnedAt (CT.SDate { day, month, year })) ->
                Inline $ whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                        [ qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
            Just _ -> Skip
            Nothing -> Skip
        Dec.KDescription -> case currentDecorator of
            Just (Dec.SDescription description) ->
                Inline $ whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
            Just _ -> Skip
            Nothing -> Skip
        Dec.KReference -> case currentDecorator of
            Just (Dec.SReference groupRef) ->
                Inline $ whenNotEditing $ renderGroupRef groupRef
            Just _ -> Skip
            Nothing -> Skip
        -- Dec.KTags -> case currentDecorator of
        --     Just (Dec.STags (Tags tags)) ->
        --         whenNotEditing $ HH.span_ $ (\tag -> itemTagBadge (conf.onTagClick tag) tag) <$> tags
        --     Just _ -> HH.text ""
        --     Nothing -> HH.text ""
    where
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
            Just encVal ->
                _editInput conf encVal
            Nothing -> nonEditing


_editInput :: forall r w i. EditableValueEvents i r -> CT.EncodedValue -> H w i
_editInput = EI.mkValueEditInput'


renderTags
    :: forall t w i
     . S.IsTag t
    => Tags t
    -> TagsRenderConfig i t
    -> H w i
renderTags tags conf =
    case conf.isEditingTags of
        Just encVal ->
            _editInput conf encVal
        Nothing ->
            HH.span_
                $ (\tag -> itemTagBadge (conf.onTagClick tag) tag) <$> Tags.toArray tags