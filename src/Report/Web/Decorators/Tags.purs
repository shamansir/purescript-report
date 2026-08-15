module Report.Web.Decorators.Tags where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Array ((:))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

import Web.UIEvent.MouseEvent (MouseEvent)

import Report.Chain (Chain(..)) as S
import Report.Class (class IsTag, class IsSortable, TagColors, tagColors, tagContent, kindOf) as S
import Report.Decorators.Tags (TagAction)
import Report.Web.CSS as CSS
import Report.Web.Helpers (H)


subjTagWrap :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagWrap tag =
    HH.span [ HP.style CSS.subjectTagWrapStyle ] [ subjTagBadge tag ]


subjTagBadge :: forall subj_tag w i. S.IsTag subj_tag => subj_tag -> H w i
subjTagBadge tag =
    HH.span
        [ HP.style CSS.tagBadgeStyle ]
        [ renderChain Nothing (S.tagColors tag) (S.tagContent tag) ]



itemTagBadge :: forall item_tag w i. S.IsTag item_tag => (MouseEvent -> i) -> item_tag -> H w i
itemTagBadge onClick tag =
    HH.span
        [ HP.style CSS.tagBadgeStyle ]
        [ renderChain (Just onClick) (S.tagColors tag) (S.tagContent tag) ]


itemTagKindBadge :: forall item_tag_kind w i. S.IsTag item_tag_kind => (MouseEvent -> i) -> item_tag_kind -> H w i
itemTagKindBadge = itemTagBadge


renderChain :: forall w i. Maybe (MouseEvent -> i) -> S.TagColors -> S.Chain String -> H w i
renderChain mbEvent tagStyle =
    case _ of
        S.End tagText ->
            HH.span
                (
                [ HP.style $ CSS.tagChainEndStyle tagStyle ]
                <> case mbEvent of
                    Just mevt -> [ HE.onClick mevt ]
                    Nothing -> []
                )
                [ HH.text tagText ]
        S.More wrapText rest ->
            HH.span
                [ HP.style $ CSS.tagChainMoreWrapStyle tagStyle ]
                [ HH.span [ HP.style CSS.tagChainMoreInnerStyle ] $ pure $ HH.text wrapText
                , HH.span [] [ renderChain mbEvent tagStyle rest ]
                ]


