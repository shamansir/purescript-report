module Report.Web.Modifiers.Tags where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.MbWrapped (MbWrapped(..)) as S
import Report.Class (class IsTag, TagColors, tagColors, tagContent) as S
import Report.Modifiers.Tags (TagAction)
import Report.Web.Helpers (H)


subjTagWrap :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagWrap tag =
    HH.span [ HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0; user-select: none;" ] [ subjTagBadge tag ]


subjTagBadge :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagBadge tag =
    HH.span
        [ HP.style "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px;" ]
        [ renderMbWrapped Nothing (S.tagColors tag) (S.tagContent tag) ]



itemTagBadge :: forall item_tag w i. S.IsTag item_tag => (MouseEvent -> i) -> item_tag -> H w i
itemTagBadge onClick tag =
    HH.span
        [ HP.style "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px;" ]
        [ renderMbWrapped (Just onClick) (S.tagColors tag) (S.tagContent tag) ]


renderMbWrapped :: forall w i. Maybe (MouseEvent -> i) -> S.TagColors -> S.MbWrapped String -> H w i
renderMbWrapped mbEvent tagStyle =
    case _ of
        S.End tagText ->
            HH.span
                (
                [ HP.style $ "padding: 1px 3px; border-radius: 4px; border: 1px solid " <> tagStyle.border <> "; opacity: 0.8; background-color: " <> tagStyle.background <> "; color: " <> tagStyle.text <> "; user-select: none;" ]
                <> case mbEvent of
                    Just mevt -> [ HE.onClick mevt ]
                    Nothing -> []
                )
                [ HH.text tagText ]
        S.More wrapText rest ->
            HH.span
                [ HP.style $ "border-radius: 4px; border: 1px solid " <> tagStyle.border <> ";" ]
                [ HH.text wrapText
                , HH.span [ HP.style "margin-left: 4px;" ] [ renderMbWrapped mbEvent tagStyle rest ]
                ]


