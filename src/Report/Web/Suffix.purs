module Report.Web.Suffix where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Map (keys, lookup) as Map
import Data.Set (toUnfoldable) as Set

import Web.UIEvent.MouseEvent (MouseEvent)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Core as CT
import Report.Class as S
import Report.Suffix (Suffixes)
import Report.Suffix as Suffixes
import Report.Suffix (Key(..), Suffix(..)) as Suffix
import Report.GroupPath (GroupPath)

import Report.Web.Modifiers.Progress (renderProgress)
import Report.Web.Modifiers.Tag (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Helpers (H, qcolorSpan, qspacerSpan, timeColor)


renderSuffixes
    :: forall @item_tag item w i
     . S.IsTag item_tag
    => S.IsItem item_tag item
    => (Suffix.Key -> MouseEvent -> i)
    -> (Suffix.Key -> Maybe CT.EncodedValue)
    -> Maybe Suffix.Key
    -> Maybe CT.EncodedValue
    -> item
    -> Array (H w i)
renderSuffixes onClick isEditingSuffix mbSelected mbEditItemName item =
    let
        i_suffixes = S.i_suffixes @item_tag item
        i_name = S.i_name @item_tag item
        suffixesKeys = Suffixes.keys i_suffixes
        isSelected suffixKey =
            case mbSelected of
                Just selectedKey -> selectedKey == suffixKey
                Nothing -> false

    in
        ( suffixesKeys
            <#> \key -> renderSuffix onClick i_suffixes (isSelected key) i_name key
        )
        <>
        pure
        ( case S.i_tags @item_tag item of
            [] -> HH.text ""
            tags -> HH.span [] $ itemTagBadge <$> tags
        )


renderSuffix
    :: forall w i
     . (Suffix.Key -> MouseEvent -> i)
    -> Suffixes
    -> Boolean
    -> String
    -> Suffix.Key
    -> H w i
renderSuffix onClick suffixes isSelected itemName key =
    let
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        wrapSuffix content =
            HH.span
                [ HP.style $ if isSelected then selectedStyle else usualStyle
                , HE.onClick $ onClick key
                ]
                [ content ]
    in
    wrapSuffix $ case key of
        Suffix.KProgress _ -> case Suffixes.get key suffixes of
            Just (Suffix.SProgress progress) -> renderProgress itemName progress
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KEarnedAt -> case Suffixes.get key suffixes of
            Just (Suffix.SEarnedAt (CT.SDate { day, month, year })) -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                    [ qcolorSpan timeColor $ CT.toLeadingZero day
                    , qspacerSpan
                    , qcolorSpan timeColor $ show month
                    , qspacerSpan
                    , qcolorSpan timeColor $ show year
                    ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KDescription -> case Suffixes.get key suffixes of
            Just (Suffix.SDescription description) -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KReference -> case Suffixes.get key suffixes of
            Just (Suffix.SReference groupRef) ->
                renderGroupRef groupRef
            Just _ -> HH.text ""
            Nothing -> HH.text ""

    {-
        [ wrapSuffix Suffix.KProgress $ case i_suffixes # Suffixes.getProgress of
            Just progress -> renderProgress (S.i_name @item_tag item) progress
            Nothing -> HH.text ""
        , wrapSuffix Suffix.KEarnedAt $ case i_suffixes # Suffixes.getEarnedAt of
            Just (CT.SDate { day, month, year }) -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ]
                    [ qcolorSpan timeColor $ CT.toLeadingZero day
                    , qspacerSpan
                    , qcolorSpan timeColor $ show month
                    , qspacerSpan
                    , qcolorSpan timeColor $ show year
                    ]
                ]
            Nothing -> HH.text ""
        , wrapSuffix Suffix.KDescription $ case i_suffixes # Suffixes.getDescription of
            Just description -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Nothing -> HH.text ""
        , wrapSuffix Suffix.KReference $ case i_suffixes # Suffixes.getReference of
            Just groupRef ->
                renderGroupRef groupRef
            Nothing -> HH.text ""
        , case S.i_tags @item_tag item of -- TODO: make tags suffixes too
            [] -> HH.text ""
            tags -> HH.span [] $ itemTagBadge <$> tags
        ]
    -}