module Report.Web.Suffix where

import Prelude

import Data.Maybe (Maybe(..))

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Report.Core as CT
import Report.Class as S
import Report.Suffix as Suffixes

import Report.Web.Modifiers.Progress (renderProgress)
import Report.Web.Modifiers.Tag (itemTagBadge)
import Report.Web.GroupPath (renderGroupRef)
import Report.Web.Helpers (H, qcolorSpan, qspacerSpan, timeColor)


renderSuffixes :: forall @item_tag w i a. S.IsTag item_tag => S.IsItem item_tag a => a -> Array (H w i)
renderSuffixes item =
    let i_suffixes = S.i_suffixes @item_tag item
    in [ case i_suffixes # Suffixes.getProgress of
            Just progress -> renderProgress (S.i_name @item_tag item) progress
            Nothing -> HH.text ""
        , case i_suffixes # Suffixes.getEarnedAt of
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
        , case i_suffixes # Suffixes.getDescription of
            Just description -> HH.span []
                [ qspacerSpan
                , HH.span [ HP.style "font-size: 0.8em; color: silver;" ] [ HH.text description ]
                ]
            Nothing -> HH.text ""
        , case i_suffixes # Suffixes.getReference of
            Just groupRef ->
                renderGroupRef groupRef
            Nothing -> HH.text ""
        , case S.i_tags @item_tag item of
            [] -> HH.text ""
            tags -> HH.span [] $ itemTagBadge <$> tags
        ]
