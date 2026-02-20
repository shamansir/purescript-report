module Report.Web.Prefix where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array as Array

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Class as S
-- import Report.Prefix as Prefixes
-- import Report.Prefix (Key(..), Prefix(..)) as Prefix
import Report.Decorator as Decorators
import Report.Modifiers.Rating (toNumber, maxValue, relValue, toStars, toString) as Rating
import Report.Modifiers.Priority (priorityChar) as Priority
import Report.Modifiers.Task (taskPToString) as Task

import Report.Web.Helpers
import Report.Web.Modifiers (PrefixesRenderConfig, PrefixRenderConfig)


renderPrefixes
    :: forall item w i
     . S.HasDecorators item
    => PrefixesRenderConfig i
    -> item
    -> Array (H w i)
renderPrefixes conf item =
    let
        i_decorators = S.i_decorators item
        prefixesKeys = Decorators.keys i_decorators >>> Array.filter Decorators.isPrefix >>> Array.sortBy Decorators.orderOf -- FIXME: should be already sorted
        isSelected prefixKey =
            case conf.mbSelectedPrefix of
                Just selectedKey -> selectedKey == prefixKey
                Nothing -> false
    in
        prefixesKeys
            <#> \key -> renderPrefix
                            { key
                            , isSelected : isSelected key
                            , isEditingPrefix : conf.isEditingPrefix key
                            , onClick : conf.onClick key
                            , onEdit : conf.onEdit key
                            , parentPrefixes : i_decorators
                            , onStartEditing : conf.onStartEditing
                            , onCancelEditing : conf.onCancelEditing
                            , noop : conf.noop
                            }


renderPrefix
    :: forall w i
     . PrefixRenderConfig i
    -> H w i
renderPrefix conf =
    let
        currentPrefix = Decorators.get conf.key conf.parentPrefixes -- FIXME: we could just filter key / value pairs out of map
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        smallerFont inside = HH.span [ HP.style "font-size: 0.6em;" ] $ pure inside
        wrapPrefix content =
            HH.span
                [ HP.style $ if conf.isSelected then selectedStyle else usualStyle
                , HE.onClick conf.onClick
                ]
                [ content ]
    in
    wrapPrefix $ case conf.key of
        Prefix.KRating -> case currentPrefix of
            Just (Prefix.PRating rating) -> HH.span_
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
        Prefix.KPriority -> case currentPrefix of
            Just (Prefix.PPriority priority) -> HH.span_
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
        Prefix.KTask -> case currentPrefix of
            Just (Prefix.PTask task) -> HH.span_
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
    where
        whenNotEditing :: H w i -> H w i
        whenNotEditing nonEditing = case conf.isEditingPrefix of
            Just (CT.EncodedValue value) ->
                HH.span [ HP.style "color: gray;" ] [ HH.text value ]
            Nothing -> nonEditing