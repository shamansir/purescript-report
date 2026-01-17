module Report.Web.Prefix where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Core.Logic (EncodedValue(..)) as CT
import Report.Class as S
import Report.Prefix as Prefixes
import Report.Prefix (Key(..), Prefix(..)) as Prefix
import Report.Modifiers.Rating (toNumber, maxValue, relValue, toStars, toString) as Rating
import Report.Modifiers.Priority (priorityChar) as Priority
import Report.Modifiers.Task (taskPToString) as Task

import Report.Web.Helpers
import Report.Web.Modifiers (PrefixesRenderConfig, PrefixRenderConfig)


renderPrefixes
    :: forall item w i
     . S.HasPrefixes item
    => PrefixesRenderConfig i
    -> item
    -> Array (H w i)
renderPrefixes conf item =
    let
        i_prefixes = S.i_prefixes item
        prefixesKeys = Prefixes.keys i_prefixes
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
                            , parentPrefixes : i_prefixes
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
        currentPrefix = Prefixes.get conf.key conf.parentPrefixes
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
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
                    in HH.span_
                        [ qcolorSpan ratingColor' $ Rating.toStars rating
                        , qthinspacerSpan
                        , qcolorSpan ratingColor' $ Rating.toString rating
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