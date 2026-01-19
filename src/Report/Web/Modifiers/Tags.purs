module Report.Web.Modifiers.Tags where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.Class (class IsTag, tagColors, tagContent) as S
import Report.Web.Helpers (H)


subjTagWrap :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagWrap tag =
    HH.span [ HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0;" ] [ subjTagBadge tag ]


subjTagBadge :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagBadge tag =
    let tagStyle = S.tagColors tag
    in HH.span
        [ HP.style $ "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; padding: 1px 3px; border-radius: 4px; border: 1px solid " <> tagStyle.border <> "; opacity: 0.8; background-color: " <> tagStyle.background <> "; color: " <> tagStyle.text <> ";" ]
        [ HH.text $ S.tagContent tag ]


itemTagBadge :: forall item_tag w i. S.IsTag item_tag => (MouseEvent -> i) -> item_tag -> H w i
itemTagBadge onClick tag =
    let tagStyle = S.tagColors tag
    in HH.span
        [ HP.style $ "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; padding: 1px 3px; border-radius: 4px; border: 1px solid " <> tagStyle.border <> "; opacity: 0.8; background-color: " <> tagStyle.background <> "; color: " <> tagStyle.text <> ";"
        , HE.onClick onClick
        ]
        [ HH.text $ S.tagContent tag ]
