module Report.Web.Decorators where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Array as Array
import Data.Tuple (Tuple(..))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Foldable (foldl)
import Data.Traversable (sequence)
import Data.Bifunctor (lmap)
import Data.Map (empty, insert) as Map

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
import Report.Decorators.Priority (priorityChar, toValue) as Priority
import Report.Decorators.Task (taskPToString) as Task
import Report.Decorators.Tags (Tags(..))
import Report.Decorators.Tags (toArray) as Tags

import Report.Web.Helpers
import Report.Web.Helpers.InlineOrBlock
import Report.Web.Decorators.Types (DecoratorsRenderConfig, DecoratorRenderConfig, TagsRenderConfig, EditableValueEvents)
import Report.Web.Decorators.Progress (renderProgress)
import Report.Web.Decorators.Tags (itemTagBadge)
import Report.Web.Decorators.Task (taskVState) as Task
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Decorators.EditInput as EI
import Report.Web.Decorators.Types


renderPrefixes
    :: forall item w i
     . S.IsItem item
    => S.HasDecorators item
    => DecoratorsRenderConfig i
    -> item
    -> VStates /\ Array (InlineOrBlock w i)
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
    -> VStates /\ Array (InlineOrBlock w i)
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
    -> VStates /\ Array (InlineOrBlock w i)
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
        _foldVStates $ decoratorsKeys
            <#> \key -> key /\
                renderDecorator
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
    -> VState /\ InlineOrBlock w i
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
        wrapRenderedDecorator = map $ mapInlineContent wrapInlineContent
        qskip = VNeutral /\ Skip
    in
    wrapRenderedDecorator $ case conf.key of
        Dec.KRating -> case currentDecorator of
            Just (Dec.PRating rating) ->
                Tuple (FromRating $ Rating.relValue rating) $
                Inline $ whenNotEditing $
                    let ratingColor' = ratingColor $ Rating.relValue rating
                    in HH.span [ HP.style "margin: 0px -4px 0px 7px; opacity: 0.9; position: relative; top: -1px;" ]
                        [ qcolorSpan ratingColor' $ Rating.toStars rating
                        , qthinspacerSpan
                        , smallerFont $ qcolorSpan ratingColor' $ Rating.toString rating
                        ]
            Just _ -> qskip
            Nothing -> qskip
        Dec.KPriority -> case currentDecorator of
            Just (Dec.PPriority priority) ->
                Tuple (FromPriority $ Priority.toValue priority) $
                Inline $ whenNotEditing $
                    let priorityColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan priorityColor' $ Priority.priorityChar priority
                        , qbracketspan "]"
                        ]
            Just _ -> qskip
            Nothing -> qskip
        Dec.KTask -> case currentDecorator of
            Just (Dec.PTask task) ->
                Tuple (FromProgress $ Task.taskVState task) $
                Inline $ whenNotEditing $
                    let taskColor' = genericColor -- TODO!
                    in HH.span_
                        [ qbracketspan "[#"
                        , qcolorSpan taskColor' $ Task.taskPToString task
                        , qbracketspan "]"
                        ]
            Just _ -> qskip
            Nothing -> qskip
        Dec.KProgress _ -> case currentDecorator of
            Just (Dec.SProgress progress) ->
                lmap FromProgress $ renderProgress
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
            Just _ -> qskip
            Nothing -> qskip
        Dec.KEarnedAt -> case currentDecorator of
            Just (Dec.SEarnedAt (CT.SDate { day, month, year })) ->
                Tuple VNeutral $
                Inline $ whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                        [ qcolorSpan timeColor $ CT.toLeadingZero day
                        , qspacerSpan
                        , qcolorSpan timeColor $ show month
                        , qspacerSpan
                        , qcolorSpan timeColor $ show year
                        ]
            Just _ -> qskip
            Nothing -> qskip
        Dec.KDescription -> case currentDecorator of
            Just (Dec.SDescription description) ->
                Tuple VNeutral $
                Inline $ whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
            Just _ -> qskip
            Nothing -> qskip
        Dec.KReference -> case currentDecorator of
            Just (Dec.SReference groupRef) ->
                Tuple VNeutral $
                Inline $ whenNotEditing $ renderGroupRef groupRef
            Just _ -> qskip
            Nothing -> qskip
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



_foldVStates :: forall w i. Array (Dec.Key /\ VState /\ InlineOrBlock w i) -> VStates /\ Array (InlineOrBlock w i)
_foldVStates = foldl foldF (Map.empty /\ []) -- sequence ??
    where
        foldF (prevVstates /\ prevIobs) (decKey /\ nextVState /\ nextIob) =
            Map.insert decKey nextVState prevVstates
            /\ Array.snoc prevIobs nextIob -- (prevVstate <> nextVState) /\ (prevIobs <> nextIobs)