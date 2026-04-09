module Report.Web.Decorators.Tags where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.Chain (Chain(..)) as S
import Report.Class (class TagAlike, class IsSortable, TagColors, tagColors, tagContent, kindOf) as S
import Report.Decorators.Tags (TagAction)
import Report.Web.Helpers (H)


subjTagWrap :: forall subj_tag w i. S.TagAlike subj_tag => subj_tag -> H w i
subjTagWrap tag =
    HH.span [ HP.style $ "display: inline-block; cursor: pointer; padding: 3px 0; user-select: none;" ] [ subjTagBadge tag ]


subjTagBadge :: forall subj_tag w i. S.TagAlike subj_tag => subj_tag -> H w i
subjTagBadge tag =
    HH.span
        [ HP.style "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; white-space: nowrap;" ]
        [ renderChain Nothing (S.tagColors tag) (S.tagContent tag) ]



itemTagBadge :: forall item_tag w i. S.TagAlike item_tag => (MouseEvent -> i) -> item_tag -> H w i
itemTagBadge onClick tag =
    HH.span
        [ HP.style "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; white-space: nowrap;" ]
        [ renderChain (Just onClick) (S.tagColors tag) (S.tagContent tag) ]


itemTagKindBadge :: forall @tag_kind item_tag w i. S.TagAlike tag_kind => S.TagAlike item_tag => S.IsSortable tag_kind item_tag => (MouseEvent -> i) -> item_tag -> H w i
itemTagKindBadge onClick tag =
    HH.span
        [ HP.style "font-size: 0.7em; position: relative; top: -1px; margin: 0 0 0 7px; white-space: nowrap;" ]
        [ renderChain (Just onClick) (S.tagColors tag) (S.tagContent $ S.kindOf @tag_kind tag) ]


renderChain :: forall w i. Maybe (MouseEvent -> i) -> S.TagColors -> S.Chain String -> H w i
renderChain mbEvent tagStyle =
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
                [ HP.style $ "border-radius: 4px; border-color: " <> tagStyle.border <> "; border-style: solid; border-width: 1px 0px 1px 1px; padding: 1px 0px 1px 4px;" ]
                [ HH.span [ HP.style "padding-right: 4px;" ] $ pure $ HH.text wrapText
                , HH.span [] [ renderChain mbEvent tagStyle rest ]
                ]


