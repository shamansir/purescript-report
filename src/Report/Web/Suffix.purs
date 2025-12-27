module Report.Web.Suffix where

import Prelude

import Data.Maybe (Maybe(..))

import Web.UIEvent.MouseEvent (MouseEvent)

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Report.Core as CT
import Report.Class as S
import Report.Suffix as Suffixes
import Report.Suffix (Key(..)) as Suffix
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
    -> Maybe Suffix.Key
    -> item
    -> Array (H w i)
renderSuffixes onClick mbSelected item =
    let
        i_suffixes = S.i_suffixes @item_tag item
        isSelected suffixKey =
            case mbSelected of
                Just selectedKey -> selectedKey == suffixKey
                Nothing -> false
        selectedStyle = "background-color: #fffdd0ff; border-radius: 5px;"
        usualStyle = "background-color: transparent;"
        wrapSuffix key content =
            HH.span
                [ HP.style $ if isSelected key then selectedStyle else usualStyle
                , HE.onClick $ onClick key
                ]
                [ content ]
    in [ wrapSuffix Suffix.KProgress $ case i_suffixes # Suffixes.getProgress of
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
