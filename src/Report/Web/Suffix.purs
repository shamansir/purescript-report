module Report.Web.Suffix where

import Prelude

import Data.Maybe (Maybe(..), maybe)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.KeyboardEvent as KE

import Report.Core (SDate(..), toLeadingZero) as CT
import Report.Core.Logic (EncodedValue(..), view, edit) as CT
import Report.Class as S
import Report.Suffix as Suffixes
import Report.Suffix (Key(..), Suffix(..)) as Suffix
import Report.Modifiers.Tags (Tags(..))

import Report.Web.Modifiers.Progress (renderProgress)
import Report.Web.Modifiers.Tags (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Helpers (H, qcolorSpan, qspacerSpan, timeColor)
import Report.Web.Modifiers (SuffixesRenderConfig, SuffixRenderConfig)


renderSuffixes
    :: forall @item_tag item w i
     . S.IsTag item_tag
    => S.IsItem item
    => S.HasSuffixes item_tag item
    => SuffixesRenderConfig i
    -> item
    -> Array (H w i)
renderSuffixes conf item =
    let
        i_suffixes = S.i_suffixes @item_tag item
        i_name = S.i_name item
        suffixesKeys = Suffixes.keys i_suffixes
        isSelected suffixKey =
            case conf.mbSelectedSuffix of
                Just selectedKey -> selectedKey == suffixKey
                Nothing -> false
    in
        suffixesKeys
            <#> \key -> renderSuffix
                            { key
                            , isSelected : isSelected key
                            , isEditingSuffix : conf.isEditingSuffix key
                            , isEditingItemName : conf.isEditingItemName
                            , onClick : conf.onClick key
                            , onEdit : conf.onEdit key
                            , onEditItemName : conf.onEditItemName
                            , parentSuffixes : i_suffixes
                            , parentItemName : i_name
                            , onStartEditing : conf.onStartEditing
                            , onCancelEditing : conf.onCancelEditing
                            , noop : conf.noop
                            }


renderSuffix
    :: forall t w i
     . S.IsTag t
    => SuffixRenderConfig i t
    -> H w i
renderSuffix conf =
    let
        currentSuffix = Suffixes.get conf.key conf.parentSuffixes
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        wrapSuffix content =
            HH.span
                [ HP.style $ if conf.isSelected then selectedStyle else usualStyle
                , HE.onClick conf.onClick
                ]
                [ content ]
    in
    wrapSuffix $ case conf.key of
        Suffix.KProgress _ -> case currentSuffix of
            Just (Suffix.SProgress progress) ->
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
                        conf.isEditingSuffix
                        progress
                    )
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KEarnedAt -> case currentSuffix of
            Just (Suffix.SEarnedAt (CT.SDate { day, month, year })) -> HH.span_
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
        Suffix.KDescription -> case currentSuffix of
            Just (Suffix.SDescription description) -> HH.span_
                [ qspacerSpan
                , whenNotEditing $
                    HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KReference -> case currentSuffix of
            Just (Suffix.SReference groupRef) ->
                whenNotEditing $ renderGroupRef groupRef
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KTags -> case currentSuffix of
            Just (Suffix.STags (Tags tags)) ->
                whenNotEditing $ HH.span_ $ itemTagBadge <$> tags
            Just _ -> HH.text ""
            Nothing -> HH.text ""
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
        whenNotEditing nonEditing = case conf.isEditingSuffix of
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