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
import Report.Modifiers.Tags (Tags(..))

import Report.Web.Modifiers.Progress (renderProgress)
import Report.Web.Modifiers.Tags (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Helpers (H, qcolorSpan, qspacerSpan, timeColor)


type SuffixesRenderConfig i item =
    { item :: item
    , mbSelectedSuffix :: Maybe Suffix.Key
    , isEditingSuffix :: Suffix.Key -> Maybe CT.EncodedValue
    , isEditingItemName :: Maybe CT.EncodedValue
    , onClick :: Suffix.Key -> MouseEvent -> i
    , onEdit :: Suffix.Key -> MouseEvent -> CT.EncodedValue -> i
    }


type SuffixRenderConfig i t =
    { key :: Suffix.Key
    , isSelected :: Boolean
    , onClick :: Suffix.Key -> MouseEvent -> i
    , onEdit :: Suffix.Key -> MouseEvent -> CT.EncodedValue -> i
    , parentSuffixes :: Suffixes t
    , parentItemName :: String
    }


renderSuffixes
    :: forall @item_tag item w i
     . S.IsTag item_tag
    => S.IsItem item_tag item
    => SuffixesRenderConfig i item
    -> Array (H w i)
renderSuffixes conf =
    let
        i_suffixes = S.i_suffixes @item_tag conf.item
        i_name = S.i_name @item_tag conf.item
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
                            , onClick : conf.onClick
                            , onEdit : conf.onEdit
                            , parentSuffixes : i_suffixes
                            , parentItemName : i_name
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
                , HE.onClick $ conf.onClick conf.key
                ]
                [ content ]
    in
    wrapSuffix $ case conf.key of
        Suffix.KProgress _ -> case currentSuffix of
            Just (Suffix.SProgress progress) -> renderProgress conf.parentItemName progress
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KEarnedAt -> case currentSuffix of
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
        Suffix.KDescription -> case currentSuffix of
            Just (Suffix.SDescription description) -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KReference -> case currentSuffix of
            Just (Suffix.SReference groupRef) ->
                renderGroupRef groupRef
            Just _ -> HH.text ""
            Nothing -> HH.text ""
        Suffix.KTags -> case currentSuffix of
            Just (Suffix.STags (Tags tags)) ->
                HH.span [] $ itemTagBadge <$> tags
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